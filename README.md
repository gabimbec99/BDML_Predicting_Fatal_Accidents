# BDML_Predicting_Fatal_Accidents
En este repositorio usted encontrará lo necesario para la replicabilidad del trabajo Predicting Fatal Accidents, realizado en el marco de la clase Big Data y Machine Learning de la Universidad de los Andes

Grupo: [Daniel Lasso](https://github.com/daniell419), [Matteo Rozo](https://github.com/MatteoRozo) y [Gabriela Mejía](https://github.com/gabimbec99).
 
Profesor: [Ignacio Sarmiento](https://github.com/ignaciomsarmiento)

El espíritu de este trabajo consiste en predecir la ubicación de los accidentes de tránsito y determinar los factores que incrementan la probabilidad de que un accidente dado cuente con al menos una víctima fatal en la ciudad de Bogotá D.C, Colombia. Por consiguiente en este repositorio usted encontrará:

## Bases de datos

En la carpeta data, se encuentran las fuentes primarias de información, siendo estas los datos de la Secretaria de Movilidad y la información extraída de la mineria de datos de condiciones climáticas. Todo estos elementos se encuentran juntos en el documento de nombre "base_final_proyecto (1).csv", donde también ya se encuentra la información de Open Street Map sobre las distancias a algunos amenities de interés.


## Scripts de código

Para realizar nuestro proyecto, se hicieron dos scripts, uno en Python y el otro en R.

El primero cuenta con la información y el proceso de mineria de datos del World Weather Online, el cual contaba con la información diaria de las características climáticas de Bogotá D.C. Así mismo, este tiene una sección de estimación por XGBoost, siendo este modelo optimizado para ejecutarlo utilizando gpu, por lo cual resulta más eficiente.

El segundo cuenta con la limpieza y procesamiento de datos recolectados, donde se especifica como se codificaron las variables y los missing values. Así mismo, este cuenta con toda la información sobre la estimación de los modelos de aprendizaje de máquinas espaciales y de clasificación. Por último, se presenta una sección de aquellos elementos gráficos y tablas utilizados en el documento.

