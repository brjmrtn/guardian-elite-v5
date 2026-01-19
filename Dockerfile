# 1. Usamos una imagen base con Maven y Java 17 para construir
FROM maven:3.8.5-openjdk-17 AS build
WORKDIR /app
COPY pom.xml .
COPY src ./src
# Compilamos y creamos el JAR
RUN mvn clean package -DskipTests

# 2. Usamos una imagen ligera solo con Java para ejecutar
FROM openjdk:17-jdk-slim
WORKDIR /app
# Copiamos el JAR que acabamos de cocinar
COPY --from=build /app/target/guardian-elite-1.0-SNAPSHOT.jar app.jar

# Avisamos a Render de que usaremos un puerto dinámico
ENV PORT=8080
EXPOSE 8080

# Arrancamos la app
CMD ["java", "-jar", "app.jar"]