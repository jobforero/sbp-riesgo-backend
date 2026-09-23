package com.tesis.resource;

import com.fasterxml.jackson.databind.JsonNode;
import com.tesis.service.MarkovService;
import jakarta.inject.Inject;
import jakarta.ws.rs.Consumes;
import jakarta.ws.rs.POST;
import jakarta.ws.rs.Path;
import jakarta.ws.rs.Produces;
import jakarta.ws.rs.core.MediaType;
import jakarta.ws.rs.core.Response;
import org.jboss.resteasy.reactive.RestForm;
import org.jboss.resteasy.reactive.multipart.FileUpload;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.StandardCopyOption;
import java.util.Map;

@Path("/api/analisis")
@Produces(MediaType.APPLICATION_JSON)
public class AnalisisResource {

    private final MarkovService markovService;

    public AnalisisResource(MarkovService markovService) {
        this.markovService = markovService;
    }

    @POST
    @Path("/upload")
    @Consumes(MediaType.MULTIPART_FORM_DATA)
    public Response subirReporte(
            @RestForm("file") FileUpload file,
            @RestForm("tipoDocumento") String tipoDocumento) {

        if (file == null) {
            return Response.status(Response.Status.BAD_REQUEST)
                    .entity(Map.of("error", "No se envió ningún archivo"))
                    .build();
        }

        try {
            File uploadDir = new File("uploads");
            if (!uploadDir.exists()) {
                uploadDir.mkdirs();
            }

            File destino = new File(uploadDir, file.fileName());
            Files.copy(file.filePath(), destino.toPath(), StandardCopyOption.REPLACE_EXISTING);

            // Ejecuta el pipeline en R y guarda en Neon
            JsonNode resultadoJson = markovService.ejecutarAnalisis(destino);

            return Response.ok(resultadoJson).build();

        } catch (Exception e) {
            return Response.status(Response.Status.INTERNAL_SERVER_ERROR)
                    .entity(Map.of("error", "Error procesando el análisis: " + e.getMessage()))
                    .build();
        }
    }
}