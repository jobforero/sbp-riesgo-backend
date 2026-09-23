package com.tesis.resource;

import com.tesis.entity.Banco;
import jakarta.ws.rs.*;
import jakarta.ws.rs.core.MediaType;
import java.util.List;

@Path("/api/bancos")
@Produces(MediaType.APPLICATION_JSON)
@Consumes(MediaType.APPLICATION_JSON)
public class BancoResource {

    @GET
    public List<Banco> listarTodos() {
        return Banco.listAll();
    }
}