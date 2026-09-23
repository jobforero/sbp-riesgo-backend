package com.tesis.model;

import io.quarkus.hibernate.orm.panache.PanacheEntity;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import org.hibernate.annotations.JdbcTypeCode;
import org.hibernate.type.SqlTypes;

import java.time.LocalDateTime;

@Entity
@Table(name = "resultado_analisis_markov")
public class ResultadoAnalisisMarkov extends PanacheEntity {

    @Column(name = "fecha_analisis")
    public LocalDateTime fechaAnalisis = LocalDateTime.now();

    @Column(name = "archivo_origen", length = 255)
    public String archivoOrigen;

    @JdbcTypeCode(SqlTypes.JSON)
    @Column(name = "matriz_probabilidades", columnDefinition = "jsonb")
    public String matrizProbabilidades;

    @Column(name = "grafico_markov_base64", columnDefinition = "TEXT")
    public String graficoMarkovBase64;

    @JdbcTypeCode(SqlTypes.JSON)
    @Column(name = "metricas", columnDefinition = "jsonb")
    public String metricas;
}