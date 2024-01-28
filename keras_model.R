library(tensorflow)
library(keras)


# define the prediction layer
PerformancePrediction(keras$layers$Layer) %py_class% {
  initialize <- function() {
    super$initialize()
  }
  call <- function(inputs) {
    tf$math$reciprocal(1 + tf$square(tf$norm(inputs$alpha - inputs$Z, axis = -1L)))
  }
}
layer_performance_prediction <- create_layer_wrapper(PerformancePrediction)


# train the model for projecting into instance/configuration space
projection_model <- function(long_metadata, layer_size, batch_size, epochs, loss_weights = list(1, 1, 1)) {
  # shuffle rows
  long_metadata <- long_metadata[sample(nrow(long_metadata)), ]
  features <- attr(long_metadata, "features")
  params <- attr(long_metadata, "params")
  n_feat <- length(features)
  # NOTE: assumes any categorical variables have been pre-processed
  n_params <- length(params)
  # projection: features
  projection_features_input <- layer_input(shape = c(n_feat))
  projection_features <- projection_features_input %>%
    layer_dense(units = layer_size) %>% layer_activation_leaky_relu() %>%
    layer_dense(units = layer_size) %>% layer_activation_leaky_relu() %>%
    layer_dense(units = layer_size) %>% layer_activation_leaky_relu() %>%
    layer_dense(units = 2)
  # NOTE: uncomment this for linear projection and comment above
  # projection_features <- projection_features_input %>% 
  #   layer_dense(units = 2)
  projector_features <- keras_model(projection_features_input, projection_features)
  projection_features_out <- projection_features %>%
    # layer_dense(units = layer_size) %>% layer_activation_leaky_relu() %>%
    layer_dense(units = n_feat, name = "features")
  # projection: parameters
  projection_params_input <- layer_input(shape = c(n_params))
  projection_params <- projection_params_input %>%
    layer_dense(units = layer_size) %>% layer_activation_leaky_relu() %>%
    layer_dense(units = layer_size) %>% layer_activation_leaky_relu() %>%
    layer_dense(units = layer_size) %>% layer_activation_leaky_relu() %>%
    layer_dense(units = 2)
  # NOTE: uncomment this for linear projection and comment above
  # projection_params <- projection_params_input %>% 
  #   layer_dense(units = 2)
  projector_params <- keras_model(projection_params_input, projection_params)
  # projection_params_out <- projection_params %>% layer_dense(units = n_params)
  projection_params_out <- projection_params %>%
    layer_dense(units = layer_size) %>% layer_activation_leaky_relu() %>%
    layer_dense(units = n_params, name = "parameters")

  output_layer <- layer_performance_prediction(list(Z = projection_features, alpha = projection_params))
  prediction_model <- keras_model(inputs = list(projector_features$input, projector_params$input),
                                  outputs = list(output_layer, projection_features_out, projection_params_out))

  compile(prediction_model, optimizer = "Adam",
          loss = list("mse", "mse", "mse"), loss_weights = loss_weights)
  prediction_model %>% fit(list(as.matrix(long_metadata[, features]),
                                as.matrix(long_metadata[, params])),
                           list(long_metadata$value, as.matrix(long_metadata[, features]), as.matrix(long_metadata[, params])),
                           batch_size = batch_size, epochs = epochs)
  
  return(list(prediction_model = prediction_model,
              projector_features_model = projector_features,
              projector_params_model = projector_params))
  
}