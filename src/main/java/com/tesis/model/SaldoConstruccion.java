package com.tesis.model;

import io.quarkus.hibernate.orm.panache.PanacheEntity;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import java.math.BigDecimal;
import java.time.LocalDate;

@Entity
@Table(name = "saldo_construccion")
public class SaldoConstruccion extends PanacheEntity {

    @Column(name = "fecha_corte", nullable = false)
    public LocalDate fechaCorte;

    @Column(name = "banco", nullable = false, length = 150)
    public String banco;

    @Column(name = "ranking")
    public Integer ranking;

    @Column(name = "total_cartera", precision = 18, scale = 2)
    public BigDecimal totalCartera;

    @Column(name = "total_construccion", precision = 18, scale = 2)
    public BigDecimal totalConstruccion;

    @Column(name = "ponderacion", precision = 10, scale = 6)
    public BigDecimal ponderacion;

    @Column(name = "vivienda_interino", precision = 18, scale = 2)
    public BigDecimal viviendaInterino;

    @Column(name = "local_comercial_interino", precision = 18, scale = 2)
    public BigDecimal localComercialInterino;

    @Column(name = "infraestructura", precision = 18, scale = 2)
    public BigDecimal infraestructura;

    @Column(name = "otras_construcciones", precision = 18, scale = 2)
    public BigDecimal otrasConstrucciones;

    // Estado calculado para Markov: TIER_1_LIDER, TIER_2_MEDIO, TIER_3_BAJO
    @Column(name = "estado_markov", length = 30)
    public String estadoMarkov;
}