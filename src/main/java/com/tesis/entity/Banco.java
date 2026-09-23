package com.tesis.entity;

import io.quarkus.hibernate.orm.panache.PanacheEntityBase;
import jakarta.persistence.*;

@Entity
@Table(name = "banco")
public class Banco extends PanacheEntityBase {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id_banco")
    public Integer id;

    @Column(name = "codigo_sbp", length = 10, unique = true)
    public String codigoSbp;

    @Column(name = "nombre", nullable = false, length = 150)
    public String nombre;

    @Column(name = "tipo_licencia", length = 50)
    public String tipoLicencia;

    @Column(name = "activo")
    public Boolean activo;
}