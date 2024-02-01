# Using the Dashboard
The "Data" folder contains an example with pre-trained models, using synthetic metadata. To use the dashboard for exploring the projected spaces, run
```
library(shiny)
runApp("app.R")
```
# Generating Synthetic Metadata
Before generating new synthetic metadata, ensure that "Data" is clear and that the working directory is the project directory. To generate synthetic metadata, run
```
source("run_generate_synthetic_experiments.R")
```
# Input Metadata
This step is not necessary if synthetic metadata has been generated. To train the projection models, first place CSV files of the following names into the "Data" folder:
- **feature_matrix.csv** $~$ The first column has the IDs of problem instances and its remaining columns have the values of instance features. An example:
```
"","F1","F2","F3","F4"
"I1",1.2,2.7,1.9,-2.1
"I2",-1.0,-0.5,-0.4,0.2
```
- **parameter_matrix.csv** $~$ The first column has the IDs of parameter configurations and its remaining columns have the values of parameters. An example:
```
"","P1","P2","P3","P4"
"C1",-0.6,-0.3,1.0,-1.2
"C2",-0.6,-0.3,0.8,-0.8
```
- **performance_matrix.csv** $~$ The first column has the IDs of problem instances and the first row has the IDs of parameter configurations (except for the first element, which is ""). This describes the performance of each configuration on each instance. This assumes, for now, that there are no missing values (imputation via empirical performance models is one way to ensure this). An example:
```
"","C1","C2"
"I1",5.64,7.01
"I2",0.03,0.24
```
- **sources_I.csv** $~$ The first column has the IDs of problem instances and there is a single column with the names of instance sources or groups. An example:
```
"","source"
"I1","-0.2 F1 + 1.19 F2 <br>+ 0.5 F3 + -2.25 F4"
"I2","-0.35 F1 + -0.49 F2 <br>+ 0.94 F3 + -1.06 F4"
```
- **sources_C.csv** $~$ The first column has the IDs of parameter configurations and there is a single column with the names of configuration sources or groups. An example:
```
"","source"
"C1","-0.35 F1 + -0.49 F2 <br>+ 0.94 F3 + -1.06 F4"
"C2","-0.35 F1 + -0.49 F2 <br>+ 0.94 F3 + -1.06 F4"
```
# Model Training
To preprocess the metadata, train the models, and to save, in "Data", what is necessary for the analysis, run
```
source("run_initialise.R")
```
# Evolving New Configurations at a Target Point
To replicate the evolution of configurations towards a target point, run
```
source("get_new_configurations.R")
```
Note that Tensorflow may introduce slight variations to the projections, dependent on computer hardware. For exact replication, replace the contents of "Data" with that of "DataBeforeNewConfigurations".
# Troubleshooting
This was tested on macOS and Windows, using R version 4.3.2 with Python 3.10.11 via the 'reticulate' package. If there are issues installing Tensorflow/Keras, Python 3.10 may work:
```
reticulate::install_python("3.10:latest")
library(keras)
install_keras()
```
If issues persist, try updating R.
