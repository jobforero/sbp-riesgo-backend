# ------------------------------------------------------------------------------
# ETAPA 1: Compilación de Quarkus con Maven
# ------------------------------------------------------------------------------
FROM maven:3.9.6-eclipse-temurin-17 AS build
WORKDIR /app

COPY pom.xml .
COPY src ./src
RUN mvn package -DskipTests

# ------------------------------------------------------------------------------
# ETAPA 2: Runtime Java 17 + R y librerías estadísticas
# ------------------------------------------------------------------------------
FROM eclipse-temurin:17-jre-jammy

# 1. Instalar R base y todos los paquetes precompilados de Ubuntu
RUN apt-get update && apt-get install -y --no-install-recommends \
    r-base \
    r-cran-readxl \
    r-cran-dplyr \
    r-cran-tidyr \
    r-cran-stringr \
    r-cran-purrr \
    r-cran-lubridate \
    r-cran-ggplot2 \
    r-cran-jsonlite \
    r-cran-base64enc \
    r-cran-markovchain \
    libxml2-dev \
    libssl-dev \
    libcurl4-openssl-dev \
    liblapack-dev \
    libblas-dev \
    gfortran \
    build-essential \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /work

# 2. Copiar los artefactos generados por Quarkus
COPY --from=build /app/target/quarkus-app/lib/ /work/lib/
COPY --from=build /app/target/quarkus-app/*.jar /work/
COPY --from=build /app/target/quarkus-app/app/ /work/app/
COPY --from=build /app/target/quarkus-app/quarkus/ /work/quarkus/

# 3. Copiar scripts y preparar directorio de subida
COPY src/main/resources/scripts/ /work/src/main/resources/scripts/
RUN mkdir -p /work/uploads

EXPOSE 8080
ENV QUARKUS_HTTP_HOST=0.0.0.0

CMD ["java", "-jar", "/work/quarkus-run.jar"]