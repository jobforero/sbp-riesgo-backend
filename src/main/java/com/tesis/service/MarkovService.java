package com.tesis.service;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.tesis.model.ResultadoAnalisisMarkov;
import jakarta.enterprise.context.ApplicationScoped;
import jakarta.inject.Inject;
import jakarta.transaction.Transactional;

import java.io.BufferedReader;
import java.io.File;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;

@ApplicationScoped
public class MarkovService {

    @Inject
    ObjectMapper objectMapper;

    // SIN @Transactional aquí para que no expire el timeout mientras R procesa
    public JsonNode ejecutarAnalisis(File archivoExcel) throws Exception {
        File scriptR = new File("src/main/resources/scripts/01_procesar_reranking.R");
        if (!scriptR.exists()) {
            throw new IllegalStateException("No se encontró el script en: " + scriptR.getAbsolutePath());
        }

        String comandoR = "C:\\Program Files\\R\\R-4.1.2\\bin\\x64\\Rscript.exe";
        if (!new File(comandoR).exists()) {
            comandoR = "C:\\Program Files\\R\\R-4.1.2\\bin\\Rscript.exe";
        }

        ProcessBuilder pb = new ProcessBuilder(
                comandoR,
                scriptR.getAbsolutePath(),
                archivoExcel.getAbsolutePath()
        );
        pb.redirectErrorStream(true);

        Process proceso = pb.start();

        StringBuilder salidaCompleta = new StringBuilder();
        StringBuilder jsonBuilder = new StringBuilder();
        boolean inicioJson = false;

        try (BufferedReader reader = new BufferedReader(new InputStreamReader(proceso.getInputStream(), StandardCharsets.UTF_8))) {
            String linea;
            while ((linea = reader.readLine()) != null) {
                salidaCompleta.append(linea).append("\n");
                if (linea.trim().startsWith("{\"status\"")) {
                    inicioJson = true;
                }
                if (inicioJson) {
                    jsonBuilder.append(linea);
                }
            }
        }

        int exitCode = proceso.waitFor();
        if (exitCode != 0 || jsonBuilder.length() == 0) {
            System.err.println("=== ERROR DESDE RSCRIPT ===");
            System.err.println(salidaCompleta);
            throw new RuntimeException("Error en R (código " + exitCode + "): " + salidaCompleta.toString().trim());
        }

        JsonNode rootNode = objectMapper.readTree(jsonBuilder.toString());

        // Guardar en la base de datos de Neon de forma transaccional aislada
        guardarResultado(archivoExcel.getName(), rootNode);

        return rootNode;
    }

    // Únicamente este método puntual maneja la transacción en PostgreSQL
    @Transactional
    public void guardarResultado(String nombreArchivo, JsonNode rootNode) {
        ResultadoAnalisisMarkov resultado = new ResultadoAnalisisMarkov();
        resultado.archivoOrigen = nombreArchivo;
        resultado.graficoMarkovBase64 = rootNode.has("grafico_markov_base64") ? rootNode.get("grafico_markov_base64").asText() : null;
        resultado.matrizProbabilidades = rootNode.has("matriz_transicion") ? rootNode.get("matriz_transicion").toString() : null;

        resultado.metricas = objectMapper.createObjectNode()
                .put("total_registros_historicos", rootNode.path("total_registros_historicos").asInt())
                .put("total_bancos_analizados", rootNode.path("total_bancos_analizados").asInt())
                .toString();

        resultado.persist();
    }
}