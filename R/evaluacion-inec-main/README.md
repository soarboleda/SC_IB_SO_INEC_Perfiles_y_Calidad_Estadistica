# Taller integrador — Encuesta socioeconómica sintética 2026

Nombres: Ivonne Benítez
        Salomé Cartagena
        Sebastián Ortiz

1. Abra `encuesta_calidad.Rproj` en RStudio.
2. Lea `GUIA_TALLER_INTEGRADOR_PASO_A_PASO.md`.
3. Complete una a una las etapas `R/00_configuracion.R` a `R/08_verificar_entregables.R`.
4. Asegure `resultados/07_paquete_reporte.rds` como entregable independiente.
5. En **Terminal** ejecute `quarto render reportes/reporte_calidad.qmd --to html`.
6. Para el sitio web, ejecute `quarto render web` y abra `web/_site/index.html`.

Se proporcionan dos archivos mensuales para comparar períodos, un CSV combinado y una tabla maestra de hogares. No use el conjunto de datos para estadísticas reales. Contiene errores deliberados. No sobrescribir `data_raw/`.

Los scripts son **solución guiada y material de referencia**, no un flujo para ejecutar todo a ciegas. La bitácora muestra propuestas no aprobadas. El informe final debe citar umbrales y denominadores.

**VISTA_PREVIA_HTML_AUTONOMA.html:** muestra visual auxiliar, generada sin R, no sustituye el informe Quarto y no forma parte de los entregables E7–E9.
