library(shiny)
library(shinythemes)
library(plotly)
library(matrixStats)
library(scales)
library(tidyr)
library(viridis)

source("parcoords.R")

# used for converting curveNumber and pointNumber to dataframe index
toidx <- function(x) {
  if (is.numeric(x)) {
    b <- rep(1, length(x))
  } else {
    stopifnot(is.factor(x))
    b <- as.numeric(x)
  }
  bord <- order(b)
  bsort <- sort(b)
  counter <- 0L
  curr <- 0L
  tb <- table(b)
  toidxmat <- matrix(NA, nrow = length(tb), ncol = max(tb))
  for (i in seq_along(bord)) {
    if (bsort[i] != curr) {
      curr <- bsort[i]
      counter <- 0L
    }
    toidxmat[curr, 1 + counter] <- bord[i]
    counter <- counter + 1
  }
  return(toidxmat)
}

attach("Data/fmat.RData")
attach("Data/fd.RData")
attach("Data/cmat.RData")
attach("Data/cd.RData")
attach("Data/pmat.RData")
attach("Data/pmat_raw.RData")
attach("Data/sources_I.RData")
attach("Data/sources_C.RData")
attach("Data/hovertextI_raw.RData")
attach("Data/hovertextI_proc.RData")
attach("Data/hovertextC_raw.RData")
attach("Data/hovertextC_proc.RData")
attach("Data/plotdata_Z.RData")
attach("Data/plotdata_alpha.RData")


# read models, for projector tool
# library(tensorflow)
# library(keras)
projector_params_model <- keras::load_model_tf("Data/projector_params_model", compile = FALSE)
load("Data/cmat_dvs.RData")
c_centers <- unlist(read.csv("Data/cmat_center.csv"))
c_scales <- unlist(read.csv("Data/cmat_scale.csv"))
c_rl <- list(c_dvs = c_dvs, centers = c_centers, scales = c_scales)
source("preprocess.R")

all_params_numeric <- all(unlist(lapply(cd, is.numeric)))

# convert non-numeric parameters to factors
for (cname in colnames(cd)) {
  if (!is.numeric(cd[[cname]])) {
    cd[[cname]] <- factor(cd[[cname]])
  }
}

# coordinate ranges
xaxmin <- min(plotdata_alpha$Z1, plotdata_Z$Z1)
yaxmin <- min(plotdata_alpha$Z2, plotdata_Z$Z2)
xaxmax <- max(plotdata_alpha$Z1, plotdata_Z$Z1)
yaxmax <- max(plotdata_alpha$Z2, plotdata_Z$Z2)
xaxrange <- xaxmax-xaxmin 
yaxrange <- yaxmax-yaxmin
xaxmin <- xaxmin - 0.05*xaxrange
xaxmax <- xaxmax + 0.05*xaxrange
yaxmin <- yaxmin - 0.05*yaxrange
yaxmax <- yaxmax + 0.05*yaxrange

# colour scales
col_funI <- col_numeric("viridis", domain = c(0, 1))
col_funC <- col_numeric("magma", domain = c(0, 1))
# simplify naming of features/parameters
colnames(fd) <- gsub("feature_", "", colnames(fd))
colnames(cd) <- gsub("parameter_", "", colnames(cd))
# get number of levels of each parameter/feature
get_number_of_levels <- function(x) {
  if (is.numeric(x)) {
    return(1L)
  } else {
    return(length(unique(x)))
  }
}
# get number of levels of dataframe d
gnol_d <- function(d) unlist(lapply(d, get_number_of_levels))
nlevels <- c(gnol_d(fd), gnol_d(cd))
# get number of instance sources
nsourcesI <- length(levels(factor(sources_I$source)))

# for converting curveNumber and pointNumber to dataframe index
toidxlist <- c(lapply(fd, toidx), lapply(cd, toidx))
toidxsources_I <- toidx(factor(sources_I$source))
toidxsources_C <- toidx(factor(sources_C$source))
point_nums_numeric_I <- t(as.matrix(seq_len(nrow(pmat_raw))))
point_nums_numeric_C <- t(as.matrix(seq_len(ncol(pmat_raw))))
# get number of configuration sources
nsourcesC <- length(levels(factor(sources_C$source)))

# data for parallel coordinates (features)
fy <- rowMeans(pmat_raw)
pc_fd <- parcoords_init(fd, fy, pmat_raw)
fdnum_initial <- pc_fd$dnum
fdimensions <- pc_fd$dimensions

# data for parallel coordinates (parameters)
cy <- colMeans(pmat_raw)
pc_cd <- parcoords_init(cd, cy, pmat_raw)
cdnum_initial <- pc_cd$dnum
cdimensions <- pc_cd$dimensions


ui <- fluidPage(
  theme = shinytheme("darkly"),
  sidebarLayout(
    sidebarPanel(
      selectInput("ichoice", "Plot (Instance Space)",
                  choices = c("Feature Distributions",
                              "Performance on each Instance (Aggregation over Selected Configurations)", "Sources")),
      conditionalPanel("input.ichoice == 'Feature Distributions'",
                       selectInput("feature", "Instance Feature",
                                   choices = gsub("feature_", "", colnames(fd)))),
      selectInput("cchoice", "Plot (Configuration Space)",
                  choices = c("Parameter Distributions",
                              "Performance by each Configuration (Aggregation over Selected Instances)", "Sources")),
      conditionalPanel("input.cchoice == 'Parameter Distributions'",
                       selectInput("parameter", "Configuration Parameter",
                                   choices = gsub("parameter_", "", colnames(cd)))),
      selectInput("sidebarplotchoice", "Sidebar Tool",
                  choices = c("Parallel Coordinates", "Configuration Projector")),
      conditionalPanel("input.sidebarplotchoice == 'Parallel Coordinates'",
                       plotlyOutput("parcoordsI", height = "27vh"),
                       plotlyOutput("parcoordsC", height = "27vh")
                       ),
      ##### NOTE: Assumes numeric parameters for now
      ##### NOTE: Round to 4 significant figures
      ##### NOTE: Uses observed mins and maxs for slider ranges
      ##### NOTE: only need sliders for variables used in projection
      if (all_params_numeric) {
      conditionalPanel("input.sidebarplotchoice == 'Configuration Projector'",
                       lapply(colnames(cd), function(nm) {
                         varmin <- signif(min(cd[[nm]]), 4)
                         varmax <- signif(max(cd[[nm]]), 4)
                         sliderInput(gsub("parameter_", "", nm), gsub("parameter_", "", nm),
                                     varmin, varmax, step = 1/100*(varmax-varmin),
                                     value = cd[[nm]][1], ticks = FALSE)
                       })
      )},
    ),
    mainPanel(
      plotlyOutput("plot", height = "100vh")
    )
  )
)


server <- function(input, output, session) {
  Z1 <- reactiveVal(plotdata_Z$Z1)
  Z2 <- reactiveVal(plotdata_Z$Z2)
  C1 <- reactiveVal(plotdata_alpha$Z1)
  C2 <- reactiveVal(plotdata_alpha$Z2)
  is_instance_selected <- reactiveVal(plotdata_Z$selected)
  is_configuration_selected <- reactiveVal(plotdata_alpha$selected)
  instance_opacities <- reactive({ifelse(is_instance_selected(), 1, 0.1)})
  configuration_opacities <- reactive({ifelse(is_configuration_selected(), 1, 0.1)})

  # data for parallel coordinates (features)
  fdnum <- reactive({
    fd1 <- fdnum_initial
    fd1$y <- rowMeans2(pmat_raw, cols = which(is_configuration_selected()))
    fd1
  })
  
  output$parcoordsI <- renderPlotly({
    d <- fdnum()
    whichidxs <- which(is_instance_selected())
    d[whichidxs, ] %>% plot_ly() %>% 
      add_trace(type = 'parcoords',
                line = list(color = ~y, showscale = T,
                            colorbar = list(title = 'Av. Metric (y)', titleside = 'right')),
                dimensions = fdimensions) %>%
      layout(title = "<b>Features</b>", margin = list(t = 0, b = 30, pad = 0))
  })
  
  # data for parallel coordinates (parameters)
  cdnum <- reactive({
    cd1 <- cdnum_initial
    cd1$y <- colMeans2(pmat_raw, rows = which(is_instance_selected()))
    cd1
  })
  
  output$parcoordsC <- renderPlotly({
    d <- cdnum()
    whichidxs <- which(is_configuration_selected())
    d[whichidxs, ] %>% plot_ly() %>%
      add_trace(type = 'parcoords', 
                line = list(color = ~y, colorscale = mapply(c, 0:99/99, rocket(100), SIMPLIFY = F),
                            showscale = T, colorbar = list(title = 'Av. Metric (y)', titleside = 'right')),
                dimensions = cdimensions) %>%
      layout(title = "<b>Parameters</b>", margin = list(t = 0, b = 30, pad = 0))
  })
  
  focus <- reactiveVal("I")  # set initial focus on I = Instance Space (C = Configuration Space)
  
  # project the parameter configuration specified with the sliders
  projected_point <- reactive({
    if (all_params_numeric) {
      xinput <- vapply(colnames(cd), function(x) input[[x]], FUN.VALUE = 1.0)
      xinput <- array(xinput, dim = c(1, length(xinput)))
      xinput <- setNames(data.frame(as.list(xinput)), c_rl$c_dvs$vars)
      xinput <- preproc_configs(xinput, initial = F, do_read = F, rl = c_rl)
      vinput <- projector_params_model(matrix(xinput, nrow=1, byrow=T))
      vinput <- as.vector(vinput)
    }
    vinput
  })
  
  # set 'value' vector by feature or performance values
  instance_values <- reactive({
    if (input$ichoice == "Feature Distributions") {
      fd[, input$feature]
    } else if (input$ichoice == "Performance on each Instance (Aggregation over Selected Configurations)") {
      rowMeans2(pmat_raw, cols = which(is_configuration_selected()))
    } else {
      factor(sources_I$source)
    }
  })
  
  # number of traces associated with 'value' vector for instances
  nlevI <- reactive({
    if (input$ichoice == "Feature Distributions") {
      nlevels[input$feature]
    } else if (input$ichoice == "Performance on each Instance (Aggregation over Selected Configurations)") {
      1L
    } else {
      nsourcesI
    }
  })
  
  # for converting curveNumber and pointNumber to dataframe index (instances)
  toidxmatI <- reactive({
    if (input$ichoice == "Feature Distributions") {
      toidxlist[[input$feature]]
    } else if (input$ichoice == "Performance on each Instance (Aggregation over Selected Configurations)") {
      point_nums_numeric_I
    } else {
      toidxsources_I
    }
  })
  
  # set 'value' vector by parameter or performance values
  configuration_values <- reactive({
    if (input$cchoice == "Parameter Distributions") {
      cvals <- cd[, input$parameter]
    } else if (input$cchoice == "Performance by each Configuration (Aggregation over Selected Instances)") {
      cvals <- colMeans2(pmat_raw, rows = which(is_instance_selected()))
    } else {
      cvals <- factor(sources_C$source)
    }
  })
  
  # number of traces associated with 'value' vector for configurations
  nlevC <- reactive({
    if (input$cchoice == "Parameter Distributions") {
      nlevels[input$parameter]
    } else if (input$cchoice == "Performance by each Configuration (Aggregation over Selected Instances)") {
      1L
    } else {
      nsourcesC
    }
  })
  
  # for converting curveNumber and pointNumber to dataframe index (configurations)
  toidxmatC <- reactive({
    if (input$cchoice == "Parameter Distributions") {
      toidxlist[[input$parameter]]
    } else if (input$cchoice == "Performance by each Configuration (Aggregation over Selected Instances)") {
      point_nums_numeric_C
    } else {
      toidxsources_C
    }
  })
  
  # main plot
  output$plot <- renderPlotly({
    if (input$ichoice == "Feature Distributions") {
      ispace_label <- input$feature
    } else if (input$ichoice == "Performance on each Instance (Aggregation over Selected Configurations)") {
      ispace_label <- "Performance Metric"  # NOTE: maybe this should be the label of the performance metric (e.g. AUC)
    }
    if (input$cchoice == "Parameter Distributions") {
      cspace_label <- input$parameter
    } else if (input$cchoice == "Performance by each Configuration (Aggregation over Selected Instances)") {
      cspace_label <- "Performance Metric"  # NOTE: maybe this should be the label of the performance metric (e.g. AUC)
    }
    # set axis labels and title based on focus()
    if (focus() == "I") {
      title.text <- '<b>Instance Space</b>'
      x.title <- '<b>Z1</b>'
      y.title <- '<b>Z2</b>'
      x2.title <- 'C1'
      y2.title <- 'C2'
      isymbol <- 'circle'
      csymbol <- 'triangle-up-open'
    } else {
      stopifnot(focus() == "C")
      title.text <- '<b>Configuration Space</b>'
      x.title <- 'Z1'
      y.title <- 'Z2'
      x2.title <- '<b>C1</b>'
      y2.title <- '<b>C2</b>'
      isymbol <- 'circle-open'
      csymbol <- 'triangle-up'
    }
    
    p <- plot_ly(hoverinfo = "text", source = "A") %>%
      layout(dragmode = 'lasso', hoverlabel = list(bgcolor = "rgb(255, 255, 255)"))
    
    # Instance Space
    if (is.numeric(instance_values())) {
      imin_val <- min(instance_values()[is_instance_selected()])
      imax_val <- max(instance_values()[is_instance_selected()])
      p <- p %>% add_trace(x = Z1(), y = Z2(), 
                           type = 'scatter', mode = 'markers',
                           showlegend = F,
                           marker = list(
                             color = instance_values(), colorscale = 'Viridis', showscale = T, symbol = isymbol,
                             cmin = imin_val,
                             cmax = imax_val,
                             opacity = instance_opacities(),
                             colorbar = list(x = 1.05, y = 0.325,
                                             title = list(text = ispace_label,
                                                          side = "right"), len = 0.3),
                             line = list(width = 0)),
                           text = hovertextI_raw, size = 1, legendgroup = "I", name = "Instances")
    } else {
      stopifnot(is.factor(instance_values()))
      cats <- levels(instance_values())
      catcols <- setNames(brewer_pal(palette = "Set2")(length(cats)), cats)  # NOTE: this assumes that the number of categories is no more than 8
      for (cat in cats) {
        whichcat <- which(instance_values() == cat)
        p <- p %>% add_trace(x = Z1()[whichcat], y = Z2()[whichcat], 
                             type = 'scatter', mode = 'markers',
                             showlegend = T,
                             marker = list(
                               color = catcols[cat], opacity = instance_opacities()[whichcat], symbol = isymbol,
                               line = list(width = 0)),
                             text = hovertextI_raw[whichcat], size = 1, legendgroup = "I", name = cat)
      }
    }
    
    # Configuration Space
    if (is.numeric(configuration_values())) {
      cmin_val <- min(configuration_values()[is_configuration_selected()])
      cmax_val <- max(configuration_values()[is_configuration_selected()])
      p <- p %>% add_trace(x = C1(), y = C2(), xaxis = "x2", yaxis = "y2",
                           type = 'scatter', mode = 'markers',
                           showlegend = F,
                           marker = list(
                             color = configuration_values(),
                             cmin = cmin_val,
                             cmax = cmax_val,
                             colorscale = mapply(c, 0:99/99, rocket(100),
                                                 SIMPLIFY = F), showscale = T,
                             opacity = configuration_opacities(),
                             colorbar = list(x = 1.05, y = 0.675,
                                             title = list(text = cspace_label, side = "right"),
                                             len = 0.3), symbol = csymbol, line = list(width = 0)),
                           text = hovertextC_raw, size = 1, legendgroup = "C", name = "Configurations")
    } else {
      stopifnot(is.factor(configuration_values()))
      cats <- levels(configuration_values())
      catcols <- setNames(brewer_pal(palette = "Set2")(length(cats)), cats)  # NOTE: this assumes that the number of categories is no more than 8
      for (cat in cats) {
        whichcat <- which(configuration_values() == cat)
        p <- p %>% add_trace(x = C1()[whichcat], y = C2()[whichcat], xaxis = "x2", yaxis = "y2",
                             type = 'scatter', mode = 'markers',
                             showlegend = T,
                             marker = list(color = catcols[cat], opacity = configuration_opacities()[whichcat], symbol = csymbol, line = list(width = 0)),
                             text = hovertextC_raw[whichcat], size = 1, legendgroup = "C", name = cat)
      }
    }
    
    if (input$sidebarplotchoice == 'Configuration Projector') {
      vinput <- projected_point()
      p <- p %>% add_trace(x = vinput[1], y = vinput[2], type = 'scatter', mode = 'markers',
                           marker = list(
                             color = "red", symbol = "circle-x-open", size = 30,
                             line = list(width = 0)), name = "Projector")
    }
    
    p <- p %>% layout(title = list(text = title.text, x = 0.05),
                      plot_bgcolor = "rgb(255, 255, 255)", paper_bgcolor = "rgb(255, 255, 255)",
                      xaxis = list(range = c(xaxmin, xaxmax), title = x.title, gridcolor = "white", zeroline = F),
                      yaxis = list(range = c(yaxmin, yaxmax), title = y.title, gridcolor = "white", zeroline = F),
                      yaxis2 = list(range = c(yaxmin, yaxmax), overlaying = "y", side = "right", title = y2.title),
                      xaxis2 = list(range = c(xaxmin, xaxmax), overlaying = "x", side = "top", title = x2.title),
                      showlegend = T, margin = list(t = 90, b = 90, pad = 10))
    
    return(p)
  })
  
  
  # change opacity and focus layer when hovering over a point
  observeEvent(event_data("plotly_hover", source = "A"), {
    ps <- event_data("plotly_hover")
    if (ps$curveNumber %in% 0:(nlevI()-1L)) {
      focus("I")
    } else if (ps$curveNumber %in% (nlevI()):(nlevI()+nlevC()-1L)) {
      focus("C")
    }
  })
  
  
  # Show feature/parameter profile graphs when selecting a group of points
  observeEvent(event_data("plotly_selected"), {
    ps <- event_data("plotly_selected")
    if (NROW(ps) > 0) {
      iselect <- is_instance_selected()
      cselect <- is_configuration_selected()
      timI <- toidxmatI()
      timC <- toidxmatC()
      if (focus() == "I") {
        iselect[] <- F
        curve0 <- which(ps$curveNumber %in% 0:(nlevI()-1L))
        if (length(curve0) > 0) {
          iselect[timI[cbind(ps[curve0,,drop=F]$curveNumber+1, 1+ps[curve0,,drop=F]$pointNumber)]] <- T
        }
      } else if (focus() == "C") {
        cselect[] <- F
        curve1 <- which(ps$curveNumber %in% (nlevI()):(nlevI()+nlevC()-1L))
        if (length(curve1) > 0) {
          cselect[timC[cbind(ps[curve1,,drop=F]$curveNumber+1-nlevI(), 1+ps[curve1,,drop=F]$pointNumber)]] <- T
        }
      }
      is_instance_selected(iselect)
      is_configuration_selected(cselect)
    }
  })
  
  
  # with 'deselect', select all points in focus layer
  observeEvent(event_data("plotly_deselect"), {
    iselect <- is_instance_selected()
    cselect <- is_configuration_selected()
    if (focus() == "I") {
      iselect[] <- T
    } else if (focus() == "C") {
      cselect[] <- T
    }
    is_instance_selected(iselect)
    is_configuration_selected(cselect)
  })
  
  # set parameter values to clicked configuration, if using projector tool
  observeEvent(event_data("plotly_click"), {
    timC <- toidxmatC()
    ps <- event_data("plotly_click")
    curve1 <- which(ps$curveNumber %in% (nlevI()):(nlevI()+nlevC()-1L))
    if (length(curve1) > 0 && input$sidebarplotchoice == 'Configuration Projector') {
      selected_config <- timC[cbind(ps[curve1,,drop=F]$curveNumber+1-nlevI(), 1+ps[curve1,,drop=F]$pointNumber)]
      selected_config_params <- cd[selected_config,,drop=F]
      for (parname in colnames(cd)) {
        # NOTE: assumes numeric parameters
        updateSliderInput(session, parname, value = selected_config_params[[parname]])
      }
    }
  })
  
}

shinyApp(ui, server)
