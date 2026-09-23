# ------------------------------------------------------------------------------
# ETAPA 1: Compilar la aplicación Quarkus con Maven
# ------------------------------------------------------------------------------
FROM maven:3.9.6-eclipse-temurin-17 AS build
WORKDIR /app

# Copiamos el descriptor del proyecto y descargamos dependencias
COPY pom.xml .
RUN mvn dependency:go-offline -B

# Copiamos el código fuente y generamos el empaquetado de Quarkus
COPY src ./src
RUN mvn package -DskipTests

# ------------------------------------------------------------------------------
# ETAPA 2: Entorno de ejecución final (Java 17 + R + Paquetes analíticos)
# ------------------------------------------------------------------------------
FROM eclipse-temurin:17-jre-jammy

# 1. Instalar el intérprete de R y dependencias de compilación en Linux
RUN apt-get update && apt-get install -y --no-install-recommends \
    r-base \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

# 2. Instalar los paquetes estadísticos en R
RUN R -e "install.packages(c('readxl', 'dplyr', 'tidyr', 'stringr', 'purrr', 'lubridate', 'markovchain', 'ggplot2', 'jsonlite', 'base64enc'), repos='https://cloud.r-project.org/')"

WORKDIR /work

# 3. Copiar el ejecutable generado por Quarkus desde la etapa 1
COPY --from=build /app/target/quarkus-app/lib/ /work/lib/
COPY --from=build /app/target/quarkus-app/*.jar /work/
COPY --from=build /app/target/quarkus-app/app/ /work/app/
COPY --from=build /app/target/quarkus-app/quarkus/ /work/quarkus/

# 4. Copiar los scripts de R al contenedor
COPY src/main/resources/scripts/ /work/src/main/resources/scripts/

# Crear carpeta para subida temporal de archivos
RUN mkdir /work/uploads

# Exponer el puerto por defecto de Quarkus
EXPOSE 8080
ENV QUARKUS_HTTP_HOST=0.0.0.0

# Comando de inicio del backend
CMD ["java", "-jar", "/work/quarkus-run.jar"]