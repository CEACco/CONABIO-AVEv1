library(shiny)
library(shinyWidgets)

library(plyr)
library(logr)
library(shinythemes)
library(raster)
library(rgdal)
library(DT)
library(shinyjs)
library(Hmisc)
library(shinybusy)
library(shinydashboard)
library(colourpicker)
library(randomcoloR)
library(FactoMineR)

ensambleNum_OG<-function(x,id){
    
    x=data.frame(val=round(x,3), id=id, stringsAsFactors=F)
    #str(x)
    #print(x)
    x=x[order(x$val,decreasing =T),]
    x=pasteFit(x$val, sep=" ")
    #print(paste("x=",x))
    return(x)
    
}

ensambleID_OG<-function(x,id){
    
    x=data.frame(val=round(x,3), id=id, stringsAsFactors=F)
    #str(tmp)
    x=x[order(x$val,decreasing =T),]
    
    x=pasteFit(x$id, sep=" ")
    #print(paste("x=",x))
    return(x)
    
}


validarFun<- function(x, wd, covVars){
    x<-read.table(paste0(wd,"/",x), header=T, sep=",")
    x<-x[which(x$Estado==1),]
    x<-x$Nombre[which(x$Tipo%in% c("integer","numeric", "factor") )] %in%covVars
    return(x)
}

sum2<-function(x){
    x=as.numeric(x)
    x=sum(x,na.rm=T)
    return(x)
}


levels2<-function(x){
    x<-length(levels(x))
    return(x)
}

EstadisticaBasica<-function(ExC, AtrCorr=NA, nfile="",digit=2){
    
    
    res<-data.frame(Variable=character(),Count=numeric(),Minimo=numeric(), Mediana=numeric(),Media=numeric(),
                    Maximo=numeric(),DesviacionEstandar=numeric(), Correlacion=numeric(), stringsAsFactors=F )
    
    
    for (i in 1:length(ExC)){
        
        dat<-ExC[c(names(ExC)[i])]
        
        dat<-na.omit(dat)
        nom<- names(ExC)[i]
        n<-dim(dat)[1]
        mins<-round(min(dat[,1]),digit)
        mdns<-round(median(dat[,1]),digit)
        means<-round(mean(dat[,1]),digit)
        maxs<-round(max(dat[,1]),digit)
        sds<-round(sd(dat[,1]),digit)
        
        if (is.na(AtrCorr)){
            cors=NA
        }
        else {
            
            dat<-data.frame(X=ExC[i],Y=ExC[AtrCorr])
            dat<-na.omit(dat)
            
            cors<-round(cor(dat[,1],dat[AtrCorr]),digit)
        }
        
        res[i,1]<-nom
        res[i,2]<-n
        res[i,3]<-mins
        res[i,4]<-mdns
        res[i,5]<-means
        res[i,6]<-maxs
        res[i,7]<-sds
        res[i,8]<-cors
        
    }
    
    write.table( res,file=nfile, sep=",", col.names=T,row.names=F)
    return(res)
    
}


ui <-
    navbarPage(title="Tareas",collapsible = F, inverse = TRUE, theme = shinytheme("sandstone"), position = "fixed-top",
               header=tags$style('body {padding-top:75px !important; padding-bottom:350 !important; height: 50px;}.navbar {min-height:25px !important;}'),
               navbarMenu("Menú", menuName = "menu", icon = icon("fas fa-bars"),   
                          tabPanel("Créditos",
                                   fluidPage(
                                       h3("Herramientas para el análisis descriptivo semi automatizado y su expresión cartográfica en relación con el medio ambiente en el que habitan 
                                          especies o asociaciones de plantas bajo el proyecto ‘Centros de origen y de diversidad genética’", align="center"), 
                                       hr(),
                                       panel(style = "overflow-y:scroll; max-height:750px; position:relative; align: centre", uiOutput("creditos"),
                                       HTML('<center><img src="640px-SEMARNAT_CONABIO_logo.png"></center>'))
                                   )
                                   ),
                          "----",
                          "Herramientas Generales",
                          tabPanel("Armonización Cartográfica",
                                   
                                   ###### tab geotiff a malla ####
                                   
                                   #fixedPage(
                                   tabsetPanel(
                                       tabPanel("Geotiff a malla",
                                                fixedPage(titlePanel(""), 
                                                          hr(),
                                                          fileInput("grd", "Malla base para la extracción", 
                                                                    accept = c("image/tiff", ".tiff"), buttonLabel="Buscar", placeholder = "---", width = '100%'),
                                                          
                                                          fluidPage(div(style="display: inline-block;vertical-align:top; width: 300px;", checkboxInput("grdVal", "¿Mantener valores de la capa?", TRUE) ),
                                                                    div(style="display: inline-block;vertical-align:top; width: 300px;", uiOutput('grdAlias') )),
                                                          
                                                          tags$hr(),
                                                          helpText("Detalles de entrada:"),
                                                          verbatimTextOutput("code"),
                                                          tags$hr(),
                                                          
                                                          fileInput("covs", "Selecciona capas para la armonización", buttonLabel = "Buscar", placeholder = "---", 
                                                                    width="100%", accept = ".tif", multiple = TRUE),
                                                          
                                                          
                                                          #fluidRow(column(12, tableOutput('covs'))),
                                                          
                                                          fixedRow(
                                                              helpText("Tabla 1. Datos de entrada para la armonización"),
                                                              column(12, align="center",
                                                                     sidebarLayout(position = "right", 
                                                                                   sidebarPanel(width = 2,
                                                                                                
                                                                                                actionButton("b1", "",icon = icon("fas fa-check"), class="btn-success"),
                                                                                                hr(),
                                                                                                actionButton("b2", "", icon = icon("fas fa-minus"), class = "btn-danger"),
                                                                                                hr(),
                                                                                                actionButton("b3", "", icon = icon("fas fa-question"), class= "btn-info" )
                                                                                                
                                                                                   ), 
                                                                                   mainPanel( width = 10, 
                                                                                             
                                                                                             dataTableOutput('dt_table'),
                                                                                             hr(),  fluidRow(column(12, align="left",helpText("Observaciones:"), verbatimTextOutput("x4")))  )
                                                                                   
                                                                     ))
                                                              
                                                          ),hr(),
                                                          fixedPage(shinyjs::useShinyjs(),
                                                                    h4("Salida"),
                                                                    
                                                                    div(style="display: inline-block;vertical-align:top; width: 100%;",  textInput("rutaSalida","Directorio:", placeholder = "No asignado", width="100%") ),
                                                                    fluidRow(    column( 9, 
                                                                                         div(style="display: inline-block;vertical-align:center;width: 100%;",  textInput("NombreSalida","Archivo:", placeholder = "No asignado", width="100%") ),
                                                                    ),
                                                                    column(3,
                                                                           div(style="display: inline-block;vertical-align:top; width: 100%;", textInput("sufijo","Sufijo", placeholder = "", width="100%")) )
                                                                    
                                                                    ),
                                                                    
                                                                    div(style="display: inline-block;vertical-align:center; width: 200px;", checkboxInput("outSHP", "¿Crear capa vectorial (shp)?", FALSE) ),
                                                                    fixedRow(            
                                                                        actionButton("run","Iniciar",width = "100%", class="btn-success")  
                                                                        ,hr(),hr(),hr(),hr() )
                                                                    
                                                          )
                                                          
                                                          
                                                )
                                                ###### 
                                       ), 
                                       tabPanel("Geotiff remuestreado",
                                                
                                                ######## tab geotif remuestreado ######
                                                fixedPage(titlePanel(""), 
                                                          hr(),
                                                          fileInput("grd2", "Malla base para la extracción", 
                                                                    accept = c("image/tiff", ".tiff"), buttonLabel="Buscar", placeholder = "---", width = '100%'),
                                                          
                                                          fluidPage(div(style="display: inline-block;vertical-align:top; width: 300px;", checkboxInput("grdVal2", "¿Mantener valores de la capa?", TRUE) ),
                                                                    div(style="display: inline-block;vertical-align:top; width: 300px;", uiOutput('grdAlias2') )),
                                                          
                                                          tags$hr(),
                                                          helpText("Detalles de entrada:"),
                                                          verbatimTextOutput("code2"),
                                                          tags$hr(),
                                                          
                                                          fileInput("covs2", "Selecciona capas para la armonización", buttonLabel = "Buscar", placeholder = "---", 
                                                                    width="100%", accept = ".tif", multiple = TRUE),
                                                          
                                                          
                                                          #fluidRow(column(12, tableOutput('covs'))),
                                                          
                                                          fixedRow(
                                                              helpText("Tabla 1. Datos de entrada para la armonización"),
                                                              column(12, align="center",
                                                                     sidebarLayout(position = "right", 
                                                                                   sidebarPanel(width = 2,
                                                                                                
                                                                                                actionButton("b1_2", "",icon = icon("fas fa-check"), class="btn-success"),
                                                                                                hr(),
                                                                                                actionButton("b2_2", "", icon = icon("fas fa-minus"), class = "btn-danger"),
                                                                                                hr(),
                                                                                                actionButton("b3_2", "", icon = icon("fas fa-question"), class= "btn-info" )
                                                                                                
                                                                                   ), 
                                                                                   mainPanel(width = 10,
                                                                                             
                                                                                             dataTableOutput('dt_table2'),
                                                                                             hr(),  fluidRow(column(12, align="left",helpText("Observaciones:"), verbatimTextOutput("x4_2")))  )
                                                                                   
                                                                     ))
                                                              
                                                          ),hr(),
                                                          fixedPage(shinyjs::useShinyjs(),
                                                                    h4("Salida"),
                                                                    
                                                                    div(style="display: inline-block;vertical-align:top; width: 100%;",  textInput("rutaSalida2","Directorio:", placeholder = "No asignado", width="100%") ),
                                                                    fluidRow(    column( 9, 
                                                                                         div(style="display: inline-block;vertical-align:center;width: 100%;",  textInput("NombreSalida2","Archivo:", placeholder = "No asignado", width="100%") ),
                                                                    ),
                                                                    column(3,
                                                                           div(style="display: inline-block;vertical-align:top; width: 100%;", textInput("sufijo2","Sufijo", placeholder = "", width="100%")) )
                                                                    
                                                                    ),
                                                                    
                                                                    div(style="display: inline-block;vertical-align:center; width: 200px;", checkboxInput("outSHP2", "¿Crear capa vectorial (shp)?", FALSE) ),
                                                                    fixedRow(            
                                                                        actionButton("run2","Iniciar",width = "100%", class="btn-success")  
                                                                        ,hr(),hr(),hr(),hr() )
                                                                    
                                                          )
                                                          
                                                          
                                                )
                                                #######      
                                       ),
                                       tabPanel("Análisis estadístico",
                                                
                                                ######## calculo stats ####
                                                fixedPage(hr(),
                                                          sidebarLayout(
                                                              sidebarPanel(width =4, shinyjs::useShinyjs(),
                                                                           fileInput("loadCovs", label="Archivo de covariables", accept =".rds", buttonLabel="Buscar", placeholder = "---", width = '100%'),
                                                                           actionButton("runStats", "Calcular Estadísticas", class="btn-success", width="100%" ), 
                                                                           hr(),
                                                                           div(style = "display:inline-block;vertical-align:top; width: 100%;", uiOutput('pickCorr')  ),
                                                                           div(style="display: inline-block;vertical-align:top; width: 100%;", textInput("statFileDir", label="Ruta de salida", value = "", width="100%") ),
                                                                           hr(),
                                                                           helpText("Resumen de la base de datos:"),
                                                                           verbatimTextOutput("str"),
                                                                           hr() ),
                                                              
                                                              mainPanel(width =8,
                                                                        helpText("Tabla 1. Estadística descriptiva de las variables numéricas"),
                                                                        dataTableOutput("statsNum"),
                                                                        hr(),
                                                                        
                                                                        helpText("Tabla 2. Estadística descriptiva de las variables categóricas"),
                                                                        dataTableOutput("statsCat"),
                                                                        hr()
                                                                        
                                                              )))     
                                                
                                                ########
                                       ),
                                       
                                       tabPanel("Visualización Gráfica",
                                                ####### visualizacion de cvos ####         
                                                fluidPage( tags$head(tags$style(HTML("hr {border-top: 1px solid #000000;}"))),
                                                           sidebarLayout(
                                                               sidebarPanel(width = 4, shinyjs::useShinyjs(),
                                                                            
                                                                            # tags$hr(style="border-color: purple;border-top: 3px solid #000000;"),
                                                                            # tags$head(tags$style(HTML("hr {border-top: 1px solid #000000;}"))),
                                                                            
                                                                            fileInput("loadCovsGraf", label="Archivo de covariables", accept =".rds", buttonLabel="Buscar", placeholder = "---", width = '100%'),
                                                                            verbatimTextOutput("strGraf"),
                                                                            hr(),
                                                                            selectInput("grafSel", label="Tipo de gráfica", choices=c("---", "Histograma","Gráfica de densidad","Gráfica de dispersión","Diagrama de caja")),
                                                                            
                                                                            wellPanel(width = 12,
                                                                                      div(style="display: inline-block;vertical-align:top; width: 80%;", uiOutput('pickValGraf') ),
                                                                                      div(style="display: inline-block;vertical-align:top; width: 40px;", colourpicker::colourInput("Xcolor", "Color", showColour ="background", value = "#2075F5")),
                                                                                      div(style="display: inline-block;vertical-align:top; width: 80%;", uiOutput("pickValejeY") )
                                                                                      
                                                                            ),
                                                                            hr(),
                                                                            
                                                                            wellPanel(
                                                                                helpText("Opciones de configuración"),
                                                                                uiOutput("xlim"),
                                                                                uiOutput("ylim"), 
                                                                                uiOutput("opcional2"),
                                                                                uiOutput("opcional1"),
                                                                                uiOutput("opcional3")
                                                                                
                                                                            ), 
                                                                            wellPanel(
                                                                                helpText("Opciones de archivo"),
                                                                                textInput("RutaGraf", label="Carpeta de salida", placeholder = "---", width="100%" ),
                                                                                textInput("fileGraf", label="Archivo de salida", placeholder = "---", width="100%" ),
                                                                                
                                                                                
                                                                                div(style="display: inline-block;vertical-align:top; width: 25%;", numericInput("altoGraf", label="Alto", value=2, min=1, max=10, step = 1)),
                                                                                div(style="display: inline-block;vertical-align:top; width: 23%;", numericInput("anchoGraf", label="Ancho", value=2, min=1, max=10, step = 1)),
                                                                                div(style="display: inline-block;vertical-align:top; width: 23%;", numericInput("PxGraf", label="Px", value=16, min=1, max=50, step = 1)),
                                                                                div(style="display: inline-block;vertical-align:top; width: 25%;", numericInput("ResGraf", label="Resolución", value=150, min=1, max=300, step = 1)),
                                                                                
                                                                                disabled( actionButton("runGraf", "Guardar en archivo", class="btn-success", width="100%") )
                                                                            )
                                                                            
                                                                            
                                                               ),
                                                               mainPanel(width = 8,
                                                                         #fixedPage(
                                                                         wellPanel(
                                                                             fixedRow(
                                                                                 helpText("Títulos"),
                                                                                 column(width = 12,
                                                                                        div(style="display: inline-block;vertical-align:top; width: 80%;", textInput("main", label="Título de la gráfica")),
                                                                                        div(style="display: inline-block;vertical-align:top; width: 10%;", numericInput("mainCex", label="Tamaño", value = 1.5, min = 0.1, step = 0.05)),
                                                                                        div(style="display: inline-block;vertical-align:top; width: 40%;", textInput("xlab", label="Título del eje X")),
                                                                                        div(style="display: inline-block;vertical-align:top; width: 40%;", textInput("ylab", label="Título del eje Y")),
                                                                                        div(style="display: inline-block;vertical-align:top; width: 10%;", numericInput("LabCex", label="Tamaño", value = 1.5, min = 0.1, step = 0.05))
                                                                                        
                                                                                        
                                                                                        
                                                                                 ),
                                                                             )
                                                                             
                                                                             
                                                                         ),
                                                                         
                                                                         wellPanel(width = 8,  height=8,
                                                                                   #tabsetPanel(id="grafTabs")
                                                                                   plotOutput("grafica", height=800))
                                                                         #)
                                                               )
                                                               
                                                               
                                                           ))
                                                
                                                
                                                
                                                
                                                
                                                #####      
                                       ),
                                       
                                       tabPanel("Manejo de datos",
                                                ####### manejo de datos ####
                                                
                                                fluidPage(shinyjs::useShinyjs(),    
                                                          wellPanel(
                                                              fileInput("covsLoadEdit", "Covariables", placeholder = "Seleccione un archivo de covariables", accept = ".rds", buttonLabel = "Buscar", width = "100%"),
                                                              selectInput("toolSelect", "Herramientas disponibles", choices = c("---","Exportar a shp","Exportar a Raster","Editar clases","Editar nombres de atributos", "Agregar covariable", "Eliminar covariable")),
                                                              fluidRow(column(12,
                                                                              div(style="display: inline-block;vertical-align:center;width: 50%", textInput("editedCovsOutDir", "Guardar en:", placeholder = "---")),
                                                                              div(style="display: inline-block;vertical-align:center;width: 30%", uiOutput("EditOpcion0") ),
                                                                              div(style="display: inline-block;vertical-align:center;width: 18%", disabled(actionButton("btn_GuardarEdit","Guardar", class="btn-primary", width = "100%")) )
                                                              )),
                                                              panel(style = "overflow-y:scroll; max-height: 200px; position:relative; align: centre",verbatimTextOutput("covsInMemory"))
                                                          ),
                                                          
                                                          wellPanel(
                                                              uiOutput("EditOpcion1"),
                                                              uiOutput("EditOpcion2"),
                                                              uiOutput("EditOpcion3"),
                                                              dataTableOutput("EditOpcionTable"),
                                                              hr(),
                                                              disabled(actionButton("btn_aplicarCambiosEdit","Ejecutar", class="btn-success", width = "100%"))
                                                              
                                                          )
                                                          
                                                )    
                                                
                                                #####
                                       )
                                   )), #)
                          
                          
                          tabPanel("Análisis de Componentes Principales",
                                   ####### PCA ####
                                   fluidPage(sidebarLayout(
                                       sidebarPanel(width=4, fluidPage(
                                           fileInput("dataPCA", label = "Datos:", multiple = F, accept = ".csv", buttonLabel = "Buscar...", placeholder = "Seleccione un archivo..."),
                                           h4("Variables disponibles:"),
                                           panel(style = "overflow-y:scroll; max-height: 600px; position:relative; align: centre", uiOutput("VarsPCAcheckbox") ),
                                           disabled(checkboxInput("dimsPCA", "¿Calcular el numero óptimo de dimensiones?", value=T)),
                                           uiOutput("nPCAs"),
                                           disabled(checkboxInput("saveSummaryPCA", "¿Guardar resumen de PCA?", value=F)),
                                           uiOutput("outDirSummaryPCA"),
                                           disabled(actionButton("btn_runPCA", label = "Calcular PCA", width = "100%", class="btn-primary"))
                                           
                                       )  
                                       ),
                                       mainPanel(width=8, fluidPage(shinyjs::useShinyjs(),
                                                                    wellPanel(
                                                                        fixedRow(column(12,
                                                                                        div(style="display: inline-block;vertical-align:top;width: 20%;",h5("Opciones de gráfica:")),
                                                                                        div(style="display: inline-block;vertical-align:top;width: 30%;",selectInput("tipoGrafPCA", label = "Tipo de gráfica", choices = c("Variables", "Individuos"), selected = "Variables")),
                                                                                        div(style="display: inline-block;vertical-align:top;width: 20%;", uiOutput("PCAdimX")),
                                                                                        div(style="display: inline-block;vertical-align:top;width: 20%;", uiOutput("PCAdimY"))
                                                                        )),
                                                                        fixedRow(column(12,
                                                                                        div(style="display: inline-block;vertical-align:top;width: 20%;",h5("Opciones segementación:")),
                                                                                        div(style="display: inline-block;vertical-align:top;width: 30%;", uiOutput("VarsPCAFact") ),
                                                                                        div(style="display: inline-block;vertical-align:top;width: 30%;", uiOutput("levelsVarsPCAFact"))
                                                                        )
                                                                        ),
                                                                        h5("Directorio de salida:"),
                                                                        fixedRow(column(12,
                                                                                        div(style="display: inline-block;vertical-align:top;width: 55%;",textInput("outDirGraphsPCA", NULL, placeholder = "---",width = "100%")) ,
                                                                                        div(style="display: inline-block;vertical-align:top;width: 20%;", disabled(actionButton("btn_graphPCA", label = "Graficar", width = "100%", class="btn-primary" )) ), 
                                                                                        div(style="display: inline-block;vertical-align:top;width: 20%;", disabled(actionButton("btn_graphPCAsave", label = "Guardar", width = "100%", class="btn-success" )))) ), 
                                                                        
                                                                    ),
                                                                    wellPanel(
                                                                        tabsetPanel(id="PCAtabs",
                                                                                    tabPanel("Gráfica",
                                                                                             plotOutput("PCAplot",   width = "100%", height = "700px")
                                                                                    ),
                                                                                    tabPanel("Resumen",
                                                                                             verbatimTextOutput("PCAsummary")
                                                                                    )
                                                                                    
                                                                        ))
                                                                    
                                       )
                                       )
                                       
                                   )
                                   )
                                       
                                   #####    
                                   ),
                          
                          tabPanel("Análisis de Agrupación",
                                   tabsetPanel(tabPanel("K-medias",
                                            fluidPage(
                                                sidebarLayout(
                                                    sidebarPanel(width=4, fluidPage(
                                                        fileInput("dataKmedias", label = "Datos:", multiple = F, accept = ".csv", buttonLabel = "Buscar...", placeholder = "Seleccione un archivo..."),
                                                        h4("Variables disponibles:"),
                                                        panel(style = "overflow-y:scroll; max-height: 500px; position:relative; align: centre", uiOutput("VarsKmediasCheckbox") ),
                                                        h5("¿Cuántos grupos clacular?"),
                                                        fixedRow(column(12,
                                                                        div(style="display: inline-block;vertical-align:top;width: 20%;", h5("Desde:")), 
                                                                        div(style="display: inline-block;vertical-align:top;width: 25%;",numericInput("MinGruposKmedias", NULL, value = 2, step = 0.5)),
                                                                        div(style="display: inline-block;vertical-align:top;width: 20%;",h5("hasta:")), 
                                                                        div(style="display: inline-block;vertical-align:top;width: 25%;",numericInput("MaxGruposKmedias", NULL, value = 5, step = 0.5))
                                                        )),
                                                        checkboxInput("saveKmedias", "¿Guardar en disco los resulatdos?", value = F),
                                                        disabled(textInput("outDirKmedias", "Directorio de salida:", placeholder = "---")),
                                                        disabled(actionButton("btn_runKmedias", "Iniciar", class="btn-primary", width="100%"))
                                                        
                                                        
                                                        
                                                    )  
                                                    ),
                                                    mainPanel(width=8, fluidPage(shinyjs::useShinyjs(),
                                                                                 wellPanel(
                                                                                     fixedRow(column(12,
                                                                                                     div(style="display: inline-block;vertical-align:top;width: 5%;", h5("Eje X: ")), 
                                                                                                     div(style="display: inline-block;vertical-align:top;width: 25%;", uiOutput("ejeXkmedias")  ),
                                                                                                     div(style="display: inline-block;vertical-align:top;width: 5%;",h5("Eje Y: ")), 
                                                                                                     div(style="display: inline-block;vertical-align:top;width: 25%;", uiOutput("ejeYkmedias") ),
                                                                                                     div(style="display: inline-block;vertical-align:top;width: 7%;",h5("Grupos: ")), 
                                                                                                     div(style="display: inline-block;vertical-align:top;width: 25%;", uiOutput("Nclusters") )
                                                                                     )),
                                                                                     h5("Directorio de salida:"),
                                                                                     fixedRow(column(12,
                                                                                                     div(style="display: inline-block;vertical-align:top;width: 50%;", textInput("outDirKmediasPlots", NULL,placeholder = "---", width = "100%") ), 
                                                                                                     div(style="display: inline-block;vertical-align:top;width: 20%;", actionButton("btn_saveKmediasGraph", "Guardar", class="btn-primary", width = "100%") ),
                                                                                                     div(style="display: inline-block;vertical-align:top;width: 20%;", disabled(actionButton("btn_plotKmediasGraph", "Graficar", class="btn-primary", width = "100%")) )
                                                                                     )),
                                                                                     wellPanel(
                                                                                         plotOutput("KmediasPlot",   width = "100%", height = "700px")
                                                                                     ))
                                                                                 
                                                    )))
                                                      
                                                      
                                                      )
                                            ))),
                          "----",
                          "Modeladción con base en hipótesis",
                          tabPanel("Preprocesamiento y Exploración",
                                   fluidPage(
                                       tabsetPanel(
                                           tabPanel("Análisis exploratorio",
                                                    ###### graficas y estadisticas #####                
                                                    fluidPage(shinyjs::useShinyjs(),
                                                              
                                                              wellPanel( 
                                                                  textInput("DirCheckExp", label="Directorio raíz", width="100%", placeholder = "---"),
                                                                  fileInput("covsPrepExp", label = "Covariables", accept = ".rds", buttonLabel="Buscar", placeholder = "---", width = '100%')
                                                              ),
                                                              
                                                              
                                                              wellPanel(
                                                                  fluidPage(
                                                                      column(6,dataTableOutput("covsExploracionIN")),
                                                                      column(1, align="center",
                                                                             fluidRow( actionButton("addVarExp", label = "", icon = icon("fas fa-plus"), class = "btn-success")),
                                                                             hr(),
                                                                             fluidRow( actionButton("RmVarExp", label = "", icon = icon("fas fa-minus"), class = "btn-success"))
                                                                      ),
                                                                      column(5,dataTableOutput("covsExploracionOUT"))
                                                                  )
                                                                  
                                                              ),
                                                              
                                                              wellPanel(
                                                                  fluidRow(
                                                                      column(3, uiOutput("checkBoxDirExp")),
                                                                      column(3, checkboxInput("rmDupExp", "¿Remover duplicados espaciales?", value = T)),
                                                                      column(2, div(style="display: inline-block;width: 100%;", disabled(actionButton("runBatchGraf", label = "Dibujar gráficas", class="btn-success", width = "100%"))  )),
                                                                      column(2, div(style="display: inline-block;width: 100%;", disabled(actionButton("runBatchStats", label = "Calcular estadísticas", class="btn-success", width = "100%")) )) 
                                                                      
                                                                  )
                                                                  
                                                              )
                                                              
                                                              #####                                        
                                                    )), 
                                           tabPanel("Visualización Gráfica (Tiempo Real)",
                                                    ###### graficas campo ####                     
                                                    sidebarLayout( sidebarPanel(width = 5,
                                                                                fluidPage(  shinyjs::useShinyjs(),
                                                                                            
                                                                                            wellPanel( 
                                                                                                textInput("DirCheck", label="Directorio raíz", width="100%", placeholder = "---"),
                                                                                                fileInput("covsPrep", label = "Covariables", accept = ".rds", buttonLabel="Buscar", placeholder = "---", width = '100%')
                                                                                            ),
                                                                                            
                                                                                            
                                                                                            wellPanel(
                                                                                                fluidPage(  
                                                                                                    
                                                                                                    #column(1),
                                                                                                    column(12,
                                                                                                           h4("Covariables disponibles:"),
                                                                                                           panel(style = "overflow-y:scroll; max-height: 250px; position:relative; align: centre", uiOutput("NumCovsUI") )
                                                                                                    )
                                                                                                    
                                                                                                    #column(2)
                                                                                                    
                                                                                                    ,
                                                                                                    
                                                                                                    
                                                                                                    column(12,
                                                                                                           h4("Directorios de trabajo:"),
                                                                                                           uiOutput("checkBoxG"),
                                                                                                           h4("Contenido de la carpeta:"),
                                                                                                           checkboxInput("rmDup", "¿Remover duplicados espaciales?", value = T),
                                                                                                           panel(style = "overflow-y:scroll; max-height: 150px; position:relative; align: centre", uiOutput("dirShps") )
                                                                                                    )
                                                                                                    
                                                                                                    
                                                                                                )
                                                                                                
                                                                                            ))), mainPanel(width = 6,
                                                                                                           h4("Compuesto gráfico:"),
                                                                                                           wellPanel(
                                                                                                               fluidRow(column(12,
                                                                                                                               panel(style = "overflow-y:scroll; max-height: 800px; position:relative; align: centre", uiOutput("plotCanvas")  ) # plotOutput("plotPrep", height = "8000px")
                                                                                                               ))
                                                                                                           )
                                                                                                           
                                                                                            )
                                                                   
                                                                   
                                                    )      
                                           )
                                           #####
                                           
                                       ))),
                          tabPanel("Preparación de datos de campo",
                                   fluidPage(
                                       tabsetPanel(
                                           tabPanel("Escritura de Hipótesis",
                                                    ###### archivos hipóteis #####                      
                                                    fluidPage(shinyjs::useShinyjs(), 
                                                              
                                                              wellPanel( 
                                                                  textInput("DirCheckHip", label="Directorio raíz", width="100%", placeholder = "---"),
                                                                  fileInput("covsPrepHip", label = "Covariables", accept = ".rds", buttonLabel="Buscar", placeholder = "---", width = '100%')
                                                              ),
                                                              
                                                              wellPanel(
                                                                  fluidPage(  
                                                                      #column(1),
                                                                      column(12,
                                                                             h4("Formulario de Hipótesis:"),
                                                                             panel(style = "overflow-y:scroll; max-height: 350px; position:relative; align: centre", dataTableOutput("HipotesisTabla") )
                                                                      )
                                                                      
                                                                  )
                                                              ),
                                                              
                                                              wellPanel(
                                                                  h4("Directorios de trabajo:"),
                                                                  uiOutput("checkBoxGHip"),
                                                                  fluidRow(    column(12,
                                                                                      div(style="display: inline-block;vertical-align:top;",helpText("Nombre de la Hipótesis: Hipotesis")) ,
                                                                                      div(style="display: inline-block;vertical-align:top;width: 300px;",textInput("sufijoHip",NULL, placeholder = "Sufijo", width="100%")),                
                                                                                      div(style="display: inline-block;vertical-align:top;", disabled(actionButton("creaHip", label = "Crear hipótesis", class="btn-success")) )
                                                                  )
                                                                  )
                                                                  
                                                              )
                                                    )                     
                                                    
                                                    
                                                    #####                        
                                           )
                                       ))),
                          tabPanel("Expresión cartográfica",
                                   fluidPage(
                                       tabsetPanel(
                                           
                                           
                                           tabPanel("Modelación con base en hipótesis",
                                                    ###### modelacion #####     
                                                    
                                                    sidebarLayout(
                                                        sidebarPanel( width = 5,    
                                                                      fluidPage(shinyjs::useShinyjs(),
                                                                                
                                                                                wellPanel( 
                                                                                    h4("Modelación por hipótesis"),hr(),hr(),
                                                                                    fileInput("covsPrepMod", label = "Covariables", accept = ".rds", buttonLabel="Buscar", placeholder = "---", width = '100%'),
                                                                                    textInput("DirCheckMod", label="Directorio raíz", width="100%", placeholder = "---"),
                                                                                    uiOutput("checkBoxGHipMod"),
                                                                                    panel(style = "overflow-y:scroll; max-height: 350px; position:relative; align: centre", verbatimTextOutput("mensajeModelos")),
                                                                                    fileInput("shpMapaMod2", label = "Complemento cartográfico", buttonLabel="Buscar", placeholder = "---", width = '100%', multiple = T),
                                                                                    #textInput("shpMapaMod2", label="Complemento cartográfico", width="100%", placeholder = "---"),
                                                                                    div(style = "margin-top: -30px"), uiOutput("mensajeShp") ,
                                                                                    
                                                                                    div(style="display: inline-block;vertical-align:top; width: 300px;", checkboxInput("rmDupMod", "¿Remover duplicados espaciales?", T) ),
                                                                                    
                                                                                    disabled( actionButton("runModelo", "Iniciar", class="btn-success", width = "100%") )  
                                                                                    
                                                                                ))),
                                                        mainPanel(width = 7, 
                                                                  fluidPage(
                                                                      wellPanel(
                                                                          h4("Simulación"),hr(),
                                                                          helpText("Hipotesis de entrada para la simulación"),
                                                                          uiOutput("checkBoxGHipSim"),
                                                                          
                                                                          panel(style = "overflow-y:scroll; max-height: 400px; position:relative; align: centre", dataTableOutput("HipotesisTablaSimulacion") ),
                                                                          
                                                                          fluidRow(    column(12,
                                                                                              div(style="display: inline-block;vertical-align:top;", disabled(actionButton("bttnSimulacion", "Aplicar cambios", class="btn-primary", width = "100%" )) ),
                                                                                              div(style="display: inline-block;vertical-align:top;",helpText("Sufijo de versión:") ),
                                                                                              div(style="display: inline-block;vertical-align:top;",textInput("SufijoSimulacion",label = NULL, width='100%' )),
                                                                                              div(style="display: inline-block;vertical-align:top;", disabled(actionButton("bttnSaveSimHip", "Guardar archivo", class="btn-primary", width = "100%" )) )
                                                                                              #,div(style="display: inline-block;vertical-align:top;",actionButton("bttnMapView", "Ver mapa", class="btn-primary", width = "100%" )) #, icon = icon("fas fa-globe")
                                                                          )),
                                                                          hr(),plotOutput("ModSimulacion", height=800)
                                                                          
                                                                          
                                                                      ))
                                                        )
                                                        
                                                    )
                                                    
                                                    
                                                    #####       
                                           ),
                                       ))),
                          tabPanel("Ensamble",
                                   ###### ensamble #####         
                                   fluidPage(shinyjs::useShinyjs(),
                                             
                                             wellPanel(
                                                 #titlePanel("Ensamble de modelos"),
                                                 textInput("DirCheckEnsamble", label="Directorio raíz", width="100%", placeholder = "---"), ##, value = "/Users/CarlosEduardo/Downloads/Documentos/2Trabajo/Colaboraciones/CONABIO/TESTs/DatosDemo/DatosCampo"
                                                 fluidRow(    column(12,
                                                                     div(style="display: inline-block;vertical-align:top;", helpText("Directorio de salida:")), 
                                                                     div(style="display: inline-block;vertical-align:top;", uiOutput("outDirEnsamble"))
                                                 ))
                                             ),
                                             
                                             splitLayout(cellWidths =c("50%", "50%"),
                                                         wellPanel(
                                                             fluidRow(
                                                                 column(5,div(style="display: inline-block;vertical-align:top; width: 100%", textInput("outDirEnsamble", label="Carpeta de salida", placeholder = "---", width='100%')) ),
                                                                 column(3,div(style="display: inline-block;vertical-align:top; width: 100%", textInput("outNameEnsamble", label="Nombre del archivo", placeholder = "---", width='100%')) ),
                                                                 column(4,div(style="display: inline-block;vertical-align:top; width: 100%", textInput("filtroEnsamble", label="Filtro", placeholder = "---", width='100%')))
                                                             ),
                                                             panel(style = "overflow-y:scroll; max-height: 450px; position:relative; align: centre",uiOutput("ListaModelos")),
                                                             div(style = "margin-top: -15px"), uiOutput("NmodelosSelected"),
                                                             disabled(actionButton("btnIniciarEnsamble", "Ensamblar modelos", class="btn-success", width="100%")) ),
                                                         wellPanel(
                                                             h4("Exportar resultados"),
                                                             hr(),
                                                             ##### remover objetos de memoria
                                                             
                                                             fileInput("ResEnsamble", "Archivo de ensamble", placeholder ="---", accept=".RData", buttonLabel = "Buscar" ), 
                                                             div(style = "margin-top: -10px"), 
                                                             uiOutput("outDirMapas"),
                                                             uiOutput("outDirMapas.new"),
                                                             uiOutput("dimensionesEnsamble"),
                                                             
                                                             checkboxInput("exportRaster", "Exportar Geotiff", value=T),
                                                             checkboxInput("exportPNG", "Exportar PNG", value=T),
                                                             checkboxInput("exportTablasEnsamble", "Exportar resumen tabular", value=F),
                                                             
                                                             fileInput("refMapasExport", "Referencia cartográfica", placeholder = "---", width = "100%", multiple = T, buttonLabel = "Buscar"),
                                                             div(style = "margin-top: -30px"), 
                                                             uiOutput("refMapasExport.name"),
                                                             div(style = "margin-bottom: 20px"), 
                                                             disabled(actionButton("btnExportarMapas", "Exportar mapas", class="btn-primary", width="100%"))
                                                             
                                                         )
                                             )
                                             
                                             
                                   )
                                   ######
                          )) 
               
               
               
    )  #### fin navbarpage

server <- function(input, output, session) {
    options(shiny.maxRequestSize=10000*1024^2) #### modifica tamaño máximo de archivo de entrada
    
  
  ####### server tab creditos ##### 
  
 output$creditos<- renderUI({HTML(paste(
    "En  <b>LA CONABIO</b>, la Coordinación de Agrobiodiversidad de la Coordinación General de Agrobiodiversidad y Recursos Biológicos colabora con la Coordinación de Planeación para el Uso de la Biodiversidad en el análisis y desarrollo de productos de información y conocimiento acerca de la agrobiodiversidad. En particular, se analiza de manera conjunta el tema de los centros de diversidad de plantas nativas domesticadas y sus parientes silvestres. <br/><br/>", 
    
    "Por otro lado, la Coordinación de Planeación para el Uso de la Biodiversidad ha desarrollado herramientas de análisis descriptivo que complementan a los análisis de distribuciones potencial basadas en modelos. Estas herramientas fueron desarrolladas para caracterizar los ambientes donde se encuentran ciertas asociaciones vegetales a partir de los puntos de colecta de varias especies de diferentes géneros de plantas. <br/><br/>",
    
    "En el diálogo continuo entre las dos coordinaciones mencionadas identificamos el potencial que tienen estas herramientas para el análisis de datos de campo de presencia de parientes silvestres de plantas cultivadas y otros recursos. <br/><br/>
Para facilitar la aplicación de los procesos de análisis y modelación, el conjunto de actividades que concluyen en la elaboración de mapas fue dividido en 5 fases o etapas. Estos procesos actualmente son independientes y requieren cierta familiaridad con el lenguaje R por parte del usuario. Se propone entonces la creación de un ambiente amigable para usuarios que no conozcan programación en R.<br/><br/> ",
    
    "<b>Objetivo </b><br/>
El objetivo de la presente aplicación es integrar las diferentes herramientas, de manera que el analista introduzca un archivo de datos de campo y pueda obtener de manera eficaz un análisis estadístico descriptivo basado en las variables disponibles.<br/>"
    
    
    ,sep=""))
  })
  
  
  
  
  
    ####### server tab geotiff a malla #####
    
    ##### impresion del resumen de raster de entrada 
    output$code <- renderPrint(
        if(!is.null(input$grd$datapath)){
            r<-raster(input$grd$datapath)
            names(r)<-input$grd$name
            r
            
        }else{"Sin información cargada"}
        
    )
    
    
    
    output$tmpGrdName<-renderText({
        if(!is.null(input$grd$datapath)){
            print(input$grd$name)
        }else{"Sin información cargada"}
        
    })
    
    
    
    ###### propuesta automatica de nombre 
    output$grdAlias <- renderUI({
        
        if(!is.null(input$grd$datapath)){
            textInput('grdAli', 'Nombre de la variable', value=gsub(".tif","",input$grd$name))
        }else{textInput('grdAli', 'Nombre de la variable', value="Sin archivo")}
        
    })
    
    
    
    ###### valores dinamicos de la tabla de entarda de covariables con fileInput()
    react_vals2 <- reactiveValues(
        df = data.frame(Geotiff=character(), Alias=character(), Tipo=character(), ValorNA=character(), Clases=character(), Resolucion=character(), Extent=character(), Ruta=character(), stringsAsFactors=F ),
        dt_row = NULL,
        add_or_edit = NULL, 
        edit_button = NULL,
        keep_track_id = nrow(df) + 1
    )
    
    
    ###### crea y agrega renglones a tabla dependiendo de fileInpus de covs
    observeEvent(input$covs, {
        temp = data.frame(Geotiff=character(), Alias=character(), Tipo=character(), ValorNA=character(), Clases=character(), Resolucion=character(), Extent=character(), Ruta=character(), stringsAsFactors=F )
        if(length(input$covs)!=0){
            
            for(i in 1:dim(input$covs)[1]){
                #print(input$covs)
                r<-raster(input$covs$datapath[i])
                temp<- data.frame(
                    Geotiff=input$covs$name[i],
                    Alias=gsub(".tif","",input$covs$name[i]),
                    Tipo="",
                    ValorNA="NA",
                    Clases="",
                    Resolucion=pasteFit(c(xres(r), yres(r)),sep=", "),
                    Extent=pasteFit(as.vector( round(extent(r),2)), sep=", "),
                    Ruta=input$covs$datapath[i], stringsAsFactors=F 
                )
                
                react_vals2$df = rbind(react_vals2$df,temp)
                
            }
        }
    })
    
    
    
    
    
    ##### crea tabla con el contenido reactivo de react_vals
    output$dt_table <- renderDataTable(
        {
            isolate(react_vals2$df)
        },
        escape = F, 
        rownames = FALSE, extensions = c("FixedColumns"), #caption = tags$caption(style="caption-side: top; text-align: center; margin: 8px 5;","Tabla 1. Datos de entrada para la armonización"),
        options = list(processing = FALSE, ordering=FALSE, paging = T,  language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'), pageLength = 5,
                       headerCallback = JS(
                           "function( thead, data, start, end, display ) {
      $(thead).closest('thead').find('th').css('background-color', '#7A1F1A');
              }"),
                       initComplete = JS(
                           "function(settings, json) {",
                           "$(this.api().table().header()).css({'color': 'white'});",
                           "}"
                       ), autoWidth = TRUE, scrollX=T, fixedColumns=list(leftColumns = 1) 
                       
        ),
        editable = list(target = "cell", disable = list(columns =c(0,5:8)))
    )
    
    #### proxy de tabla para el manejo interno 
    proxy <- dataTableProxy("dt_table") 
    observe({
        replaceData(proxy, react_vals2$df, resetPaging = FALSE, rownames = FALSE)
    })
    
    
    #### evento elimina renglon
    observeEvent(input$b2,{
        if(!is.null(input$dt_table_rows_selected)){
            
            elim<-react_vals2$df[input$dt_table_rows_selected, ]
            output$x4<-renderPrint({cat("Se eliminó(aron) el/los registro(s):\n\n")
                elim[,1:5]})
            
            react_vals2$df <- react_vals2$df[-input$dt_table_rows_selected, ]
        }
        reset("dt_table_rows_selected")
    })
    
    #### evento para check entradas
    observeEvent(input$b1,{
        # print(names(react_vals2$df))
        # print(react_vals2$df)
        show_modal_spinner()
        
        if(is.null(input$grd$datapath)){
            
            showModal( modalDialog(h2("¡Error!"), helpText("Seleccione una capa base para la extracción") ,footer=modalButton("Cerrar"), size="m", ))
            
        }else{
            
            ## extent base de comparación
            r<-raster(input$grd$datapath)
            names(r)<-input$grd$name
            pol1<-as(extent(r), "SpatialPolygons")
            
            res.chk<-data.frame(Alias=character(), Nombre=character(), Tipo=character(), Extent=character(), Factores=character(), stringsAsFactors=F)
            
            if(dim(react_vals2$df)[1]!=0){
                
                for(i in 1:dim(react_vals2$df)[1]){
                    r.tmp<-raster(react_vals2$df$Ruta[i])
                    pol.tmp<-as(extent(r.tmp), "SpatialPolygons")
                    if( !is.null(intersect(pol1,pol.tmp)) ){
                        tmp.chk<-data.frame(
                            Alias=react_vals2$df$Alias[i],
                            Nombre="",
                            Tipo="",
                            Extent="Válido",
                            Factores=""
                            , stringsAsFactors=F)
                    }else{
                        
                        tmp.chk<-data.frame(
                            Alias=react_vals2$df$Alias[i],
                            Nombre="",
                            Tipo="",
                            Extent="Sin Intersección",
                            Factores=""
                            , stringsAsFactors=F)
                    }
                    res.chk<-rbind(res.chk, tmp.chk)
                }
                
                ###### valida alias
                res.chk$Nombre <- "Válido"
                res.chk$Nombre[res.chk$Alias == ""] <- "¡Error!"
                
                ###### verifica entradas para el tipo de variable
                res.chk$Tipo[react_vals2$df$Tipo %in% c("1","Continuo","continuo", "2", "Categorico", "categorico")] <- "Válido"
                res.chk$Tipo[!react_vals2$df$Tipo %in% c("1","Continuo","continuo", "2", "Categorico", "categorico")] <- "Valor no válido"
                
                
                #### verifica el numero de factores length( strsplit(info$value,",")[[1]] )
                
                for(i in 1:dim(react_vals2$df)[1]){
                    # print(paste("for: ", i))
                    r.tmp<-raster(react_vals2$df$Ruta[i])
                    
                    if(react_vals2$df$Tipo[i] %in% c("2", "Categorico", "categorico")){
                        r.tmp.u<-length(unique(r.tmp))
                        if(react_vals2$df$Clases[i]==""){
                            res.chk$Factores[i]<-"Se mantendrán valores originales"
                        }else if((length( strsplit(react_vals2$df$Clases[i],",")[[1]] ) == r.tmp.u)){
                            res.chk$Factores[i]<-"Dimensiones válidas"
                        }else if(!(length( strsplit(react_vals2$df$Clases[i],",")[[1]] ) == r.tmp.u)){
                            res.chk$Factores[i]<-paste0("Advertencia. Diferentes dimensiones: En Geotiff: ",r.tmp.u ," Introducidas: ",length( strsplit(react_vals2$df$Clases[i],",")[[1]] ))
                        }else{res.chk$Factores[i]<-"----"}
                    }else if(react_vals2$df$Tipo[i]==""){res.chk$Factores[i]<-"No ha definido el tipo de variable"}else{res.chk$Factores[i]<-"Es una variable continua"}
                    
                }
                
                output$chk <- renderDT(res.chk, editable=F, 
                                       options = list(processing = FALSE, ordering=FALSE, paging = TRUE,  language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'), pageLength = 5,
                                                      autoWidth = F, scrollX=T)
                )
                
                remove_modal_spinner()
                showModal( modalDialog(h3("Reporte de entrada"), hr(), DTOutput("chk",), footer=modalButton("Cerrar"), size="m", ))
                
            }else{
                showModal( modalDialog(h2("¡Error!"), helpText("Seleccione alguna covariable") ,footer=modalButton("Cerrar"), size="m", ))
            }    
            
        }##### fin if/else que valida q grd ha sido cargdo
    })
    
    ######## evento para la actualizacion de edicion de celda
    observeEvent(input$dt_table_cell_edit,{
        ind = input$dt_table_cell_edit
        #str(info)
        i = ind$row
        j = ind$col+1
        z = ind$value
        #print(paste0(i,"-",j,"-",z))
        react_vals2$df[i, j] <- isolate(z)
    })
    
    ####### boton de ayuda
    observeEvent(input$b3,{
        
        output$helpText <- renderUI({
            HTML(paste("La sección “Datos de entrada para la armonización” (Tabla 1) 
                                    se generan de manera automática cada vez que el usuario ingresa 
                                    una nueva capa armonizable. Los campos <b>'Geotiff'</b>,  <b>'Resolución'</b>, <b>'Extent'</b> y <b>'Ruta'</b> no aceptan edición.<br/><br/>", 
                       "Los campos necesarios para el computo son:<br/><br/>", 
                       "<b>Alias</b>: El nombre final que cada variables tendrá al finalizar el cómputo. Este valor <FONT color='red'><b>no acepta Vacíos</b></FONT>.<br/>
<b>Tipo</b>: El tipo de variable; Continua o Categórica. Para las variables continuas las potenciales entradas son: <FONT color='green'>“1”, “Continuo” o “continuo”</FONT>; para las variables categóricas: <FONT color='green'>“2”, “Categorico”, “categorico”</FONT> (sin acentos).</b><br/>
<b>ValorNA</b>': El valor por el que los datos vacíos serán remplazados para cada variable. Si el valor es <b>vacío</b> o <b>“NA”</b>, estos será removidos.<br/>
<b>Clases</b>: Si la variables es categórica, sus clases (niveles) serán renombrados por los introducidos en este campo. <b>Se recomineda q la dimensión del vector de datos introducidos por el usuario sea de la misma longitud que el numero de valores únicos en el Geotiff</b>. Las nuevas clases deben estar separadas por <b>“,”</b>; las equivalencias de cada valor del Geotiff y la etiqueta a asignar deberan ser separa das por <b>':'</b>  (ej. Valor1:Clase1,Valor2:Clase2...). <br/> 
",
"<br/><br/>Recomendaciones generales:<br/><br/>
-Evite uso de acentos y caracteres especiales (ej. @#$%)<br/>
-Se recomienda el uso de nombres no muy largos<br/>
-Si el usuario desea la exportación en formato vectorial (shp), los nombres de las columnas serán abreviado.<br/>",
sep=""))
        })
        
        showModal( modalDialog(h2("Consideraciones"),
                               htmlOutput("helpText"),
                               footer=modalButton("Cerrar"), size="m", ))
    })
    
    
    
    
    toListen1 <- reactive({
        list(react_vals2$df,input$grd,input$rutaSalida,input$NombreSalida, input$grdAli)
    })
    
    
    ######## activa boton/desactiva para iniciar proceso 
    observeEvent(toListen1(),{
        control.cond <- all(!is.null(input$grd$datapath),
                            react_vals2$df$Alias!="",
                            react_vals2$df$Tipo%in%c("1","Continuo","continuo", "2", "Categorico", "categorico"),
                            react_vals2$df$ValorNA!="", dim(react_vals2$df)[1]!=0,
                            input$rutaSalida!="",
                            input$NombreSalida!="",
                            input$grdAli != ""
        )
        if(control.cond){
            shinyjs::enable("run")
        }else{shinyjs::disable("run")}
    })
    
    
    ########## evento para verificar en tiempo real entrada de campos ligeros
    observeEvent(input$dt_table_cell_edit,{
        
        info<-input$dt_table_cell_edit
        
        if(any(info$col==c(1,2,3,4))){
            switch(info$col,
                   "1"={ if(info$value == ""){ chk="Alias no puede estar vacío." }else{ chk=paste0("Alias cambiado por: ", info$value) } },
                   "2"={ 
                       if(info$value %in% c(1, "Continuo", "continuo") ){ chk=paste0("Se configuró a ", react_vals2$df[info$row, 1] ," de tipo 'Continuo'") 
                       }else if(info$value %in% c(2, "Categorico", "categorico")){ chk=paste0("Se configuró a ", react_vals2$df[info$row, 1] ," de tipo 'Categórico'")
                       }else{ chk= paste("Valor no permitido. Opciones;\n para varibles continuas: 1, Continuo, continuo;\n para variables categóricas: 2, Categorico, categorico") } 
                   },
                   "3"={
                       if(info$value!=""){ chk=paste0("Los valores vacíos para ", react_vals2$df[info$row, 1], " serán convertidos a: ", info$value) 
                       }else{ chk= "El valor para los NA no puede estar vacío, si desea NO conservarlos asigne 'NA'"}
                   },
                   "4"={ 
                       if( info$value != "" ){chk= paste0("Se registró(aron) ", length( strsplit(info$value,",")[[1]] ), " nueva(s) clase(s) para la variable ", react_vals2$df[info$row, 1] )
                       }else{chk=paste0("Se mantendrán los valores originales de ", react_vals2$df[info$row, 1], " como clases")}
                   }
                   
            )}else{print("valor no valido")}
        output$x4<-renderPrint({cat(chk)})
        
    })
    
    
    
    
    
    ####### correr proceso
    observeEvent(input$run,{
        
        if(input$sufijo==""){sufijo=format(Sys.time(), "_%d-%m-%Y_%Hh%Mm%Ss")}else{sufijo=input$sufijo}  ##### Sys.getenv("LOGNAME")????
        
        if(!dir.exists(input$rutaSalida)){
            
            showModal( modalDialog(h2("¡Error!"),
                                   helpText("El directorio de salida no existe o no se puede tener acceso") ,
                                   footer=modalButton("Cerrar"), size="m", ) )
            
        }else{
            
            dir.create(paste(input$rutaSalida, "/", input$NombreSalida, sufijo , sep=""))
            
            withProgress(message = "Compilando", value = 0, {
                show_modal_spinner()
                
                
                log.tmp <- file.path(paste(gsub(" ","",input$rutaSalida),"/",input$NombreSalida,sufijo,"/",input$NombreSalida,sufijo,".log",sep=""))
                lf <- log_open(log.tmp, show_notes=F, logdir=F)
                
                log_print("Ruta de salida:", console =F)
                log_print(paste(input$rutaSalida, "/", input$NombreSalida, sufijo , sep=""), console =F)
                
                dat<-raster(input$grd$datapath)
                names(dat)<-input$grdAli
                
                log_print("--------- Método: Geotiff a malla ---------", console =F)
                log_print("Raster base:", console =F)
                log_print(dat, console =F)
                
                log_print("Opciones de entrada:", console =F)
                log_print(print(react_vals2$df[,1:7]), console =T)
                
                
                dat<-as.data.frame(dat, xy=T)
                dat<-na.omit(dat)
                
                
                
                ####### mantiene o no el valor del grid
                if(!input$grdVal){
                    #print("if true")
                    dat<-dat[,c("x","y")]
                    # print(head(dat))
                    
                }
                
                n<-dim(react_vals2$df)[1]+1
                
                ## for para la extraccion
                for(i in 1:dim(react_vals2$df)[1]){
                    
                    incProgress(1/n, detail = paste("Fase:", i, "/", n ))
                    
                    #       print("entra for")
                    r.tmp<-raster(react_vals2$df$Ruta[i])
                    dat[react_vals2$df$Alias[i]]<-extract(r.tmp, dat[,c("x","y")])
                    
                    
                    ############ if
                    
                    if(react_vals2$df$Tipo[i] %in% c("2", "Categorico", "categorico")){
                        
                        clases<-  unlist( strsplit(react_vals2$df$Clases[i],",") ) 
                        
                        
                        for(c in clases){
                            c.tmp<-unlist(strsplit(c,":"))
                            
                            dat[which( dat[react_vals2$df$Alias[i]] == c.tmp[1] ), react_vals2$df$Alias[i]] <- c.tmp[2]
                            
                        }
                        
                        if( !(react_vals2$df$ValorNA[i] %in% c("","NA")) ){
                            
                            dat[is.na(dat[,react_vals2$df$Alias[i]]), react_vals2$df$Alias[i]] <- react_vals2$df$ValorNA[i]
                            dat[react_vals2$df$Alias[i]]<-as.factor(dat[,react_vals2$df$Alias[i]])
                            
                        }else{
                            
                            dat[react_vals2$df$Alias[i]] <- as.factor( dat[,react_vals2$df$Alias[i]] )
                            
                        }
                        
                        
                    }else{ ### si son continuas solo sustituye el NA
                        
                        
                        if( !(react_vals2$df$ValorNA[i] %in% c("","NA")) ){
                            
                            dat[is.na(dat[,react_vals2$df$Alias[i]]), react_vals2$df$Alias[i]] <- as.numeric(react_vals2$df$ValorNA[i])
                            
                        }
                        
                    }
                    
                }
                
                dat<-na.omit(dat)
                saveRDS(dat,paste(gsub(" ","",input$rutaSalida),"/",input$NombreSalida,sufijo,"/",input$NombreSalida,sufijo,".rds",sep=""))
                
                log_print("Resumen de resultado:", console =F)
                log_print(print(ls.str( pattern="dat"  )), console =T)
                
                
                
                nam<-names(dat)[lapply(dat,class)!="factor"]
                nam
                
                if(length(nam)!=0){
                    log_print("-------------", console =F)
                    log_print("Para las variables continuas:", console =F)
                    log_print(summary(dat[,c(nam[c(-1,-2)])]), console =F)
                }
                
                
                nam<-names(dat)[lapply(dat,class)=="factor"]
                nam
                
                if(length(nam)!=0){
                    log_print("-------------", console =F)
                    log_print("Para las variables categóricas:", console =F)
                    for(i in 1:length(nam)){
                        log_print(nam[i], console =F)
                        log_print(tapply(dat[,nam[i]], dat[,nam[i]], length), console =F)
                    }
                    
                }
                
                
                
                log_print("Archivo(s) creado(s):", console =F)
                log_print(paste(gsub(" ","",input$rutaSalida),"/",input$NombreSalida,sufijo,"/",input$NombreSalida,sufijo,".rds",sep=""), console =F)
                
                ####### salva shp
                if(input$outSHP){
                    dat.geo<-dat
                    coordinates(dat.geo) <- ~x+y
                    shapefile(dat.geo,paste(gsub(" ","",input$rutaSalida),"/",input$NombreSalida,sufijo,"/",input$NombreSalida,sufijo,".shp",sep=""), overwrite=T)
                    log_print(paste(gsub(" ","",input$rutaSalida),"/",input$NombreSalida,sufijo,"/",input$NombreSalida,sufijo,".shp",sep=""), console =F)
                    
                }
                
                
                log_close()
                
            })##### fin barra de progreso
            
            remove_modal_spinner()
            
        } ## fin else de if de directorio
        
        #print("fin.....")
        
    }) ###### RUN Button
    
    ###### 
    
    ####### server tab geotiff remuestreado #####
    
    ##### impresion del resumen de raster de entrada 
    output$code2 <- renderPrint(
        if(!is.null(input$grd2$datapath)){
            r<-raster(input$grd2$datapath)
            names(r)<-input$grd2$name
            r
            
        }else{"Sin información cargada"}
        
    )
    
    
    
    output$tmpGrdName2<-renderText({
        if(!is.null(input$grd2$datapath)){
            print(input$grd2$name)
        }else{"Sin información cargada"}
        
    })
    
    
    
    ###### propuesta automatica de nombre 
    output$grdAlias2 <- renderUI({
        
        if(!is.null(input$grd2$datapath)){
            textInput('grdAli2', 'Nombre de la variable', value=gsub(".tif","",input$grd2$name))
        }else{textInput('grdAli2', 'Nombre de la variable', value="Sin archivo")}
        
    })
    
    
    
    ###### valores dinamicos de la tabla de entarda de covariables con fileInput()
    react_vals <- reactiveValues(
        df = data.frame(Geotiff=character(), Alias=character(), Tipo=character(), ValorNA=character(), Clases=character(), Resolucion=character(), Extent=character(), Ruta=character(), stringsAsFactors=F ),
        dt_row = NULL,
        add_or_edit = NULL, 
        edit_button = NULL,
        keep_track_id = nrow(df) + 1
    )
    
    
    ###### crea y agrega renglones a tabla dependiendo de fileInpus de covs
    observeEvent(input$covs2, {
        temp = data.frame(Geotiff=character(), Alias=character(), Tipo=character(), ValorNA=character(), Clases=character(), Resolucion=character(), Extent=character(), Ruta=character(), stringsAsFactors=F )
        if(length(input$covs2)!=0){
            
            for(i in 1:dim(input$covs2)[1]){
                #print(input$covs)
                r<-raster(input$covs2$datapath[i])
                temp<- data.frame(
                    Geotiff=input$covs2$name[i],
                    Alias=gsub(".tif","",input$covs2$name[i]),
                    Tipo="",
                    ValorNA="NA",
                    Clases="",
                    Resolucion=pasteFit(c(xres(r), yres(r)),sep=", "),
                    Extent=pasteFit(as.vector( round(extent(r),2)), sep=", "),
                    Ruta=input$covs2$datapath[i], stringsAsFactors=F 
                )
                
                react_vals$df = rbind(react_vals$df,temp)
                
            }
        }
    })
    
    
    
    
    
    ##### crea tabla con el contenido reactivo de react_vals
    output$dt_table2 <- renderDataTable(
        {
            isolate(react_vals$df)
        },
        escape = F, 
        rownames = FALSE, extensions = c("FixedColumns"), #caption = tags$caption(style="caption-side: top; text-align: center; margin: 8px 5;","Tabla 1. Datos de entrada para la armonización"),
        options = list(processing = FALSE, ordering=F, paging = T,  language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'), pageLength = 5,
                       headerCallback = JS(
                           "function( thead, data, start, end, display ) {
      $(thead).closest('thead').find('th').css('background-color', '#7A1F1A');
              }"),
                       initComplete = JS(
                           "function(settings, json) {",
                           "$(this.api().table().header()).css({'color': 'white'});",
                           "}"
                       ), autoWidth = TRUE, scrollX=T, fixedColumns=list(leftColumns = 1) 
                       
        ),
        editable = list(target = "cell", disable = list(columns =c(0,5:8)))
    )
    
    #### proxy de tabla para el manejo interno 
    proxy2 <- dataTableProxy("dt_table2") 
    observe({
        replaceData(proxy2, react_vals$df, resetPaging = FALSE, rownames = FALSE)
    })
    
    
    #### evento elimina renglon
    observeEvent(input$b2_2,{
        if(!is.null(input$dt_table2_rows_selected)){
            
            elim<-react_vals$df[input$dt_table2_rows_selected, ]
            output$x4_2<-renderPrint({cat("Se eliminó(aron) el/los registro(s):\n\n")
                elim[,1:5]})
            
            react_vals$df <- react_vals$df[-input$dt_table2_rows_selected, ]
        }
        reset("dt_table2_rows_selected")
    })
    
    #### evento para check entradas
    observeEvent(input$b1_2,{
        # print(names(react_vals$df))
        # print(react_vals$df)
        show_modal_spinner()
        
        if(is.null(input$grd2$datapath)){
            
            showModal( modalDialog(h2("¡Error!"), helpText("Seleccione una capa base para la extracción") ,footer=modalButton("Cerrar"), size="m", ))
            
        }else{
            
            ## extent base de comparación
            r<-raster(input$grd2$datapath)
            names(r)<-input$grd2$name
            pol1<-as(extent(r), "SpatialPolygons")
            
            res.chk<-data.frame(Alias=character(), Nombre=character(), Tipo=character(), Extent=character(), Factores=character(), stringsAsFactors=F)
            
            if(dim(react_vals$df)[1]!=0){
                
                for(i in 1:dim(react_vals$df)[1]){
                    r.tmp<-raster(react_vals$df$Ruta[i])
                    pol.tmp<-as(extent(r.tmp), "SpatialPolygons")
                    if( !is.null(intersect(pol1,pol.tmp)) ){
                        tmp.chk<-data.frame(
                            Alias=react_vals$df$Alias[i],
                            Nombre="",
                            Tipo="",
                            Extent="Válido",
                            Factores=""
                            , stringsAsFactors=F)
                    }else{
                        
                        tmp.chk<-data.frame(
                            Alias=react_vals$df$Alias[i],
                            Nombre="",
                            Tipo="",
                            Extent="Sin Intersección",
                            Factores=""
                            , stringsAsFactors=F)
                    }
                    res.chk<-rbind(res.chk, tmp.chk)
                }
                
                ###### valida alias
                res.chk$Nombre <- "Válido"
                res.chk$Nombre[res.chk$Alias == ""] <- "¡Error!"
                
                ###### verifica entradas para el tipo de variable
                res.chk$Tipo[react_vals$df$Tipo %in% c("1","Continuo","continuo", "2", "Categorico", "categorico")] <- "Válido"
                res.chk$Tipo[!react_vals$df$Tipo %in% c("1","Continuo","continuo", "2", "Categorico", "categorico")] <- "Valor no válido"
                
                
                #### verifica el numero de factores length( strsplit(info$value,",")[[1]] )
                
                for(i in 1:dim(react_vals$df)[1]){
                    # print(paste("for: ", i))
                    r.tmp<-raster(react_vals$df$Ruta[i])
                    
                    if(react_vals$df$Tipo[i] %in% c("2", "Categorico", "categorico")){
                        r.tmp.u<-length(unique(r.tmp))
                        if(react_vals$df$Clases[i]==""){
                            res.chk$Factores[i]<-"Se mantendrán valores originales"
                        }else if((length( strsplit(react_vals$df$Clases[i],",")[[1]] ) == r.tmp.u)){
                            res.chk$Factores[i]<-"Dimensiones válidas"
                        }else if(!(length( strsplit(react_vals$df$Clases[i],",")[[1]] ) == r.tmp.u)){
                            res.chk$Factores[i]<-paste0("Advertencia. Diferentes dimensiones: En Geotiff: ",r.tmp.u ," Introducidas: ",length( strsplit(react_vals$df$Clases[i],",")[[1]] ))
                        }else{res.chk$Factores[i]<-"----"}
                    }else if(react_vals$df$Tipo[i]==""){res.chk$Factores[i]<-"No ha definido el tipo de variable"}else{res.chk$Factores[i]<-"Es una variable continua"}
                    
                }
                
                output$chk2 <- renderDT(res.chk, editable=F, 
                                        options = list(processing = FALSE, ordering=FALSE, paging = TRUE,  language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'), pageLength = 5,
                                                       autoWidth = F, scrollX=T)
                )
                
                remove_modal_spinner()
                showModal( modalDialog(h3("Reporte de entrada"), hr(), DTOutput("chk2",), footer=modalButton("Cerrar"), size="m", ))
                
            }else{
                showModal( modalDialog(h2("¡Error!"), helpText("Seleccione alguna covariable") ,footer=modalButton("Cerrar"), size="m", ))
            }    
            
        }##### fin if/else que valida q grd ha sido cargdo
    })
    
    ######## evento para la actualizacion de edicion de celda
    observeEvent(input$dt_table2_cell_edit,{
        ind = input$dt_table2_cell_edit
        #str(info)
        i = ind$row
        j = ind$col+1
        z = ind$value
        #print(paste0(i,"-",j,"-",z))
        react_vals$df[i, j] <- isolate(z)
    })
    
    ####### boton de ayuda
    observeEvent(input$b3_2,{
        
        output$helpText2 <- renderUI({
            HTML(paste("La sección “Datos de entrada para la armonización” (Tabla 1) 
                                    se generan de manera automática cada vez que el usuario ingresa 
                                    una nueva capa armonizable. Los campos <b>'Geotiff'</b>,  <b>'Resolución'</b>, <b>'Extent'</b> y <b>'Ruta'</b> no aceptan edición.<br/><br/>", 
                       "Los campos necesarios para el computo son:<br/><br/>", 
                       "<b>Alias</b>: El nombre final que cada variables tendrá al finalizar el cómputo. Este valor <FONT color='red'><b>no acepta Vacíos</b></FONT>.<br/>
<b>Tipo</b>: El tipo de variable; Continua o Categórica. Para las variables continuas las potenciales entradas son: <FONT color='green'>“1”, “Continuo” o “continuo”</FONT>; para las variables categóricas: <FONT color='green'>“2”, “Categorico”, “categorico”</FONT> (sin acentos).</b><br/>
<b>ValorNA</b>: El valor por el que los datos vacíos serán remplazados para cada variable. Si el valor es <b>vacío</b> o <b>“NA”</b>, estos será removidos.<br/>
<b>Clases</b>: Si la variables es categórica, sus clases (niveles) serán renombrados por los introducidos en este campo. <b>Se recomineda q la dimensión del vector de datos introducidos por el usuario sea de la misma longitud que el numero de valores únicos en el Geotiff</b>. Las nuevas clases deben estar separadas por <b>“,”</b>; las equivalencias de cada valor del Geotiff y la etiqueta a asignar deberan ser separa das por <b>':'</b>  (ej. Valor1:Clase1,Valor2:Clase2...). <br/> 
",
"<br/><br/>Recomendaciones generales:<br/><br/>
-Evite uso de acentos y caracteres especiales (ej. @#$%)<br/>
-Se recomienda el uso de nombres no muy largos<br/>
-Si el usuario desea la exportación en formato vectorial (shp), los nombres de las columnas serán abreviado.<br/>",
sep=""))
        })
        
        showModal( modalDialog(h2("Consideraciones"),
                               htmlOutput("helpText2"),
                               footer=modalButton("Cerrar"), size="m", ))
    })
    
    
    
    
    toListen <- reactive({
        list(react_vals$df,input$grd2,input$rutaSalida2,input$NombreSalida2, input$grdAli2)
    })
    
    
    ######## activa boton/desactiva para iniciar proceso 
    observeEvent(toListen(),{
        control.cond <- all(!is.null(input$grd2$datapath),
                            react_vals$df$Alias!="",
                            react_vals$df$Tipo%in%c("1","Continuo","continuo", "2", "Categorico", "categorico"),
                            react_vals$df$ValorNA!="", dim(react_vals$df)[1]!=0,
                            input$rutaSalida2!="",
                            input$NombreSalida2!="",
                            input$grdAli2 != ""
        )
        if(control.cond){
            shinyjs::enable("run2")
        }else{shinyjs::disable("run2")}
    })
    
    
    ########## evento para verificar en tiempo real entrada de campos ligeros
    observeEvent(input$dt_table2_cell_edit,{
        
        info<-input$dt_table2_cell_edit
        
        if(any(info$col==c(1,2,3,4))){
            switch(info$col,
                   "1"={ if(info$value == ""){ chk="Alias no puede estar vacío." }else{ chk=paste0("Alias cambiado por: ", info$value) } },
                   "2"={ 
                       if(info$value %in% c(1, "Continuo", "continuo") ){ chk=paste0("Se configuró a ", react_vals$df[info$row, 1] ," de tipo 'Continuo'") 
                       }else if(info$value %in% c(2, "Categorico", "categorico")){ chk=paste0("Se configuró a ", react_vals$df[info$row, 1] ," de tipo 'Categórico'")
                       }else{ chk= paste("Valor no permitido. Opciones;\n para varibles continuas: 1, Continuo, continuo;\n para variables categóricas: 2, Categorico, categorico") } 
                   },
                   "3"={
                       if(info$value!=""){ chk=paste0("Los valores vacíos para ", react_vals$df[info$row, 1], " serán convertidos a: ", info$value) 
                       }else{ chk= "El valor para los NA no puede estar vacío, si desea NO conservarlos asigne 'NA'"}
                   },
                   "4"={ 
                       if( info$value != "" ){chk= paste0("Se registró(aron) ", length( strsplit(info$value,",")[[1]] ), " nueva(s) clase(s) para la variable ", react_vals$df[info$row, 1] )
                       }else{chk=paste0("Se mantendrán los valores originales de ", react_vals$df[info$row, 1], " como clases")}
                   }
                   
            )}else{print("valor no valido")}
        output$x4_2<-renderPrint({cat(chk)})
        
    })
    
    
    
    
    
    ####### correr proceso
    observeEvent(input$run2,{
        
        if(input$sufijo2==""){sufijo=format(Sys.time(), "_%d-%m-%Y_%Hh%Mm%Ss")}else{sufijo=input$sufijo2}  ##### Sys.getenv("LOGNAME")????
        
        if(!dir.exists(input$rutaSalida2)){
            
            showModal( modalDialog(h2("¡Error!"),
                                   helpText("El directorio de salida no existe o no se puede tener acceso") ,
                                   footer=modalButton("Cerrar"), size="m", ) )
            
        }else{
            
            dir.create(paste(input$rutaSalida2, "/", input$NombreSalida2, sufijo , sep=""))
            
            withProgress(message = "Compilando", value = 0, {
                show_modal_spinner()
                
                
                log.tmp <- file.path(paste(gsub(" ","",input$rutaSalida2),"/",input$NombreSalida2,sufijo,"/",input$NombreSalida2,sufijo,".log",sep=""))
                lf <- log_open(log.tmp, show_notes=F, logdir=F)
                
                log_print("Ruta de salida:", console =F)
                log_print(paste(input$rutaSalida2, "/", input$NombreSalida2, sufijo , sep=""), console =F)
                
                
                n<-(2*dim(react_vals$df)[1])+1  #### para la barra de progreso
                
                BaseGrd<-raster(input$grd2$datapath)
                names(BaseGrd)<-input$grdAli2
                
                resStack<-stack()
                resStack
                
                ipb<-0
                ### for para crear stack, resampleando rasters 
                for(i in 1:dim(react_vals$df)[1]){
                    
                    r.tmp<-raster(react_vals$df$Ruta[i])
                    names(r.tmp)<-react_vals$df$Alias[i]
                    
                    if( class( try( stack(BaseGrd, r.tmp), silent=T ) )=="try-error" ){
                        
                        if( react_vals$df$Tipo[i] %in% c("2", "Categorico", "categorico")){
                            r.tmp<-resample(r.tmp, BaseGrd, method="ngb")
                            
                        }else{
                            r.tmp<-resample(r.tmp, BaseGrd, method="bilinear")
                        }
                        
                        resStack<-stack(resStack, r.tmp)
                        
                    }else{
                        resStack<-stack(resStack, r.tmp)
                    }
                    ipb<-ipb+1
                    incProgress(1/n, detail = paste("Fase:", ipb, "/", n ))
                    
                }
                
                
                resStack<-stack(BaseGrd,resStack)
                dat<-as.data.frame(resStack,xy=T)
                
                
                #### for para cambio de valores factoriales y NAs
                for(i in 1:dim(react_vals$df)[1]){
                    
                    if( react_vals$df$Tipo[i] %in% c("2", "Categorico", "categorico")){
                        
                        clases<-  unlist( strsplit(react_vals$df$Clases[i],",") ) 
                        
                        for(c in clases){
                            c.tmp<-unlist(strsplit(c,":"))
                            
                            dat[which( dat[react_vals$df$Alias[i]] == c.tmp[1] ), react_vals$df$Alias[i]] <- c.tmp[2]
                            
                        }
                        
                        if( !(react_vals$df$ValorNA[i] %in% c("","NA")) ){
                            
                            dat[is.na(dat[,react_vals$df$Alias[i]]), react_vals$df$Alias[i]] <- react_vals$df$ValorNA[i]
                            dat[react_vals$df$Alias[i]]<-as.factor(dat[,react_vals$df$Alias[i]])
                            
                        }else{
                            
                            dat[react_vals$df$Alias[i]] <- as.factor( dat[,react_vals$df$Alias[i]] )
                            
                        }
                        
                    }else{ ### si son continuas solo sustituye el NA
                        
                        if( !(react_vals$df$ValorNA[i] %in% c("","NA")) ){
                            
                            dat[is.na(dat[,react_vals$df$Alias[i]]), react_vals$df$Alias[i]] <- as.numeric(react_vals$df$ValorNA[i])
                            
                        }
                        
                    }
                    
                    ipb<-ipb+1
                    incProgress(1/n, detail = paste("Fase:", ipb, "/", n ))
                    
                }
                
                
                log_print("--------- Método: Geotiff remuestreado ---------", console =F)
                log_print("Raster base:", console =F)
                log_print(BaseGrd, console =F)
                
                log_print("Opciones de entrada:", console =F)
                log_print(print(react_vals$df[,1:7]), console =T)
                
                ###### mantiene o no el valor del grid
                if(!input$grdVal2){
                    #print("if true")
                    dat<-dat[,-3]
                    # print(head(dat))
                    
                }
                
                
                dat<-na.omit(dat)
                
                
                saveRDS(dat,paste(gsub(" ","",input$rutaSalida2),"/",input$NombreSalida2,sufijo,"/",input$NombreSalida2,sufijo,".rds",sep=""))
                
                log_print("Resumen de resultado:", console =F)
                log_print(print(ls.str( pattern="dat"  )), console =T)
                
                
                
                nam<-names(dat)[lapply(dat,class)!="factor"]
                nam
                
                if(length(nam)!=0){
                    log_print("-------------", console =F)
                    log_print("Para las variables continuas:", console =F)
                    log_print(summary(dat[,c(nam[c(-1,-2)])]), console =F)
                }
                
                
                nam<-names(dat)[lapply(dat,class)=="factor"]
                nam
                
                if(length(nam)!=0){
                    log_print("-------------", console =F)
                    log_print("Para las variables categóricas:", console =F)
                    for(i in 1:length(nam)){
                        log_print(nam[i], console =F)
                        log_print(tapply(dat[,nam[i]], dat[,nam[i]], length), console =F)
                    }
                    
                }
                
                
                
                log_print("Archivo(s) creado(s):", console =F)
                log_print(paste(gsub(" ","",input$rutaSalida2),"/",input$NombreSalida2,sufijo,"/",input$NombreSalida2,sufijo,".rds",sep=""), console =F)
                
                ####### salva shp
                if(input$outSHP2){
                    dat.geo<-dat
                    coordinates(dat.geo) <- ~x+y
                    shapefile(dat.geo,paste(gsub(" ","",input$rutaSalida2),"/",input$NombreSalida2,sufijo,"/",input$NombreSalida2,sufijo,".shp",sep=""), overwrite=T)
                    log_print(paste(gsub(" ","",input$rutaSalida2),"/",input$NombreSalida2,sufijo,"/",input$NombreSalida2,sufijo,".shp",sep=""), console =F)
                    
                }
                
                
                
                log_close()
                
            })##### fin barra de progreso
            
            remove_modal_spinner()
            
        } ## fin else de if de directorio
        
        
    })###### RUN Button
    
    ######
    
    ####### server tab stats ####  
    
    ######## inicializacion
    covs <- reactiveValues(df=data.frame(Clase=character(), Conteo=numeric()))
    
    output$statsNum <- renderDataTable( data.frame(Clase=character(), Conteo=numeric()),
                                        options = list(ordering=F, language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'), 
                                                       headerCallback = JS(
                                                         "function( thead, data, start, end, display ) {
      $(thead).closest('thead').find('th').css('background-color', '#7A1F1A');
              }"),
                                                       initComplete = JS(
                                                         "function(settings, json) {",
                                                         "$(this.api().table().header()).css({'color': 'white'});",
                                                         "}"
                                                       ) ) )  
    
    
    output$statsCat <- renderDataTable( data.frame(Clase=character(), Conteo=numeric()),
                                        options = list(ordering=F, language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'), 
                                                       headerCallback = JS(
                                                         "function( thead, data, start, end, display ) {
      $(thead).closest('thead').find('th').css('background-color', '#7A1F1A');
              }"),
                                                       initComplete = JS(
                                                         "function(settings, json) {",
                                                         "$(this.api().table().header()).css({'color': 'white'});",
                                                         "}"
                                                       )) )
    
    
    observeEvent(input$loadCovs, {
        output$statsNum <- renderDataTable( data.frame(Clase=character(), Conteo=numeric()),
                                            options = list(ordering=F, language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'), 
                                                           headerCallback = JS(
                                                             "function( thead, data, start, end, display ) {
      $(thead).closest('thead').find('th').css('background-color', '#7A1F1A');
              }"),
                                                           initComplete = JS(
                                                             "function(settings, json) {",
                                                             "$(this.api().table().header()).css({'color': 'white'});",
                                                             "}"
                                                           ) ) )  
        
        
        output$statsCat <- renderDataTable( data.frame(Clase=character(), Conteo=numeric()),
                                            options = list(ordering=F, language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'), 
                                                           headerCallback = JS(
                                                             "function( thead, data, start, end, display ) {
      $(thead).closest('thead').find('th').css('background-color', '#7A1F1A');
              }"),
                                                           initComplete = JS(
                                                             "function(settings, json) {",
                                                             "$(this.api().table().header()).css({'color': 'white'});",
                                                             "}"
                                                           )) )  
        
    })
    
    toListen3 <- reactive({
        list(input$loadCovs,input$statFileDir)
    })
    
    observeEvent(toListen3(), {
        cond.tmp<-all(input$statFileDir != "", !is.null(input$loadCovs$datapath) )
        if(cond.tmp){
            enable("runStats")
        }else{
            disable("runStats")
        }})
    
    
    
    output$str<-renderPrint({
        #print(getwd())
        
        if( !is.null( input$loadCovs$datapath ) ){
            show_modal_spinner()
            covs$df<- readRDS(input$loadCovs$datapath)
            
            str(covs$df )
            remove_modal_spinner()  
            
        }else{"Sin ningún dato cargado"}
        
    })
    
    ###### selector de correlacion 
    output$pickCorr <- renderUI({
        
        if(!is.null(input$loadCovs$datapath)){
            selectInput('pickCorrUI', "Correlación con:", choices = c("---", names(covs$df)[lapply(covs$df,class)!="factor"]), width = "100%")
        }else{selectInput('pickCorrUI', "Correlación con:", choices = c("---"), width = "100%" )}
        
    })
    
    
    observeEvent(input$runStats, {
        
        
        if(!dir.exists(input$statFileDir)){
            
            showModal( modalDialog(h2("¡Error!"),
                                   helpText("El directorio de salida no existe o no se puede tener acceso") ,
                                   footer=modalButton("Cerrar"), size="m", ) )
            
        }else{
            
            show_modal_spinner()
            
            # print("entro")
            
            num<- as.data.frame( covs$df[,lapply(covs$df,class)!="factor"] )
            names(num) <- names(covs$df)[lapply(covs$df,class)!="factor"]
            
            fact<- as.data.frame( covs$df[,lapply(covs$df,class)=="factor"] )
            names(fact) <- names(covs$df)[lapply(covs$df,class)=="factor"]
            
            # 
            # print(str(num))
            # print(str(fact))
            
            
            
            if(dim(num)[2]>0){
                
                if( input$pickCorrUI == "---" ){
                    
                    # print("if")
                    # print(input$pickCorrUI)
                    
                    output$statsNum <- renderDataTable({
                        isolate(EstadisticaBasica(num, nfile = paste0(input$statFileDir,"/" ,gsub(".rds","_NumStats.csv", input$loadCovs$name )) ) )
                    }, 
                    escape = F, selection = 'none',
                    rownames = FALSE, extensions = c("FixedColumns"),
                    options = list(processing = FALSE, ordering=T, paging = T, pageLength = 5, language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'), 
                                   headerCallback = JS(
                                     "function( thead, data, start, end, display ) {
      $(thead).closest('thead').find('th').css('background-color', '#7A1F1A');
              }"),
                                   initComplete = JS(
                                     "function(settings, json) {",
                                     "$(this.api().table().header()).css({'color': 'white'});",
                                     "}"
                                   ), autoWidth = F, scrollX=T, fixedColumns=list(leftColumns = 1) )
                    )
                    
                }else{
                    
                    # print("else")
                    # print(input$pickCorrUI)
                    
                    output$statsNum <- renderDataTable({
                        isolate(EstadisticaBasica( num , AtrCorr=input$pickCorrUI, nfile = paste0(input$statFileDir,"/" ,gsub(".rds","_NumStats.csv",input$loadCovs$name)) ) )
                    },
                    escape = F, selection = 'none',
                    rownames = FALSE, extensions = c("FixedColumns"),
                    options = list(processing = FALSE, ordering=T, paging = T, pageLength = 5, language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                                   headerCallback = JS(
                                     "function( thead, data, start, end, display ) {
      $(thead).closest('thead').find('th').css('background-color', '#7A1F1A');
              }"),
                                   initComplete = JS(
                                     "function(settings, json) {",
                                     "$(this.api().table().header()).css({'color': 'white'});",
                                     "}"
                                   ), autoWidth = F, scrollX=T, fixedColumns=list(leftColumns = 1))
                    )
                    
                    
                }
                
            }
            ######## para categoricos
            
            if(dim(fact)[2]>0){
                
                ### tabla de conteos por clase
                maxLev<-max(unlist(lapply(fact, levels2)))
                
                tmp<-data.frame(id=1:maxLev)
                tmp
                
                
                #### crea tamplate de tabla
                sketch = htmltools::withTags(table(
                    class = 'display',
                    thead(
                        tr(
                            lapply(names(fact), th, colspan=2)
                        ),
                        tr(
                            lapply(rep(c('Clase', 'Conteo'), dim(fact)[2]), th)
                        )
                    )))
                
                
                for(i in 1:dim(fact)[2]){
                    t<-tapply(fact[,i], fact[,i], length)
                    
                    tmp[paste0("Clase_",i)]<-NA
                    tmp[1:dim(t)[1],paste0("Clase_",i)] <- names(t)
                    
                    tmp[paste0("Conteo_",i)]<-NA
                    tmp[1:dim(t)[1],paste0("Conteo_",i)] <- as.vector(t)
                    
                }
                
                output$statsCat <- renderDataTable(
                    datatable(tmp[,-1], rownames = FALSE, escape = F, selection = 'none', container = sketch, options = list( ordering=T, language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'), 
                                                                                                                              initComplete = JS("function(settings, json) {","$(this.api().table().header()).css({'background-color': '#7A1F1A', 'color': 'white'});","}"), 
                                                                                                                              paging = T, autoWidth = F, scrollX=T, pageLength = 5) )
                    
                )
                
                
                tmp.1<- rbind(names(tmp), tmp)
                tmp.1<-rbind(NA, tmp.1)
                tmp.1[1,seq(from=2, to=dim(fact)[2]*2, by=2)]<-names(fact)
                tmp.1[is.na(tmp.1)]<-""
                
                write.table(tmp.1[,-1], paste0(input$statFileDir,"/" ,gsub(".rds","_CatStats.csv",input$loadCovs$name)), sep=",", col.names=F, row.names=F )
                
            }
            
            remove_modal_spinner()
        }
        
    })
    
    
    #####  
    
    ###### server graficas covs #####  
    
    h<-reactiveValues(fig="")
    covsGraf<-reactiveValues(df=data.frame(Variable=character(), Value=numeric()) )
    
    
    output$strGraf<-renderPrint({
        #print(getwd())
        
        if( !is.null( input$loadCovsGraf$datapath ) ){
            show_modal_spinner()
            covsGraf$df<- readRDS(input$loadCovsGraf$datapath)
            
            cat(paste0("Datos cargados con exito:\n",
                       "Se caragron ", dim(covsGraf$df)[1], " datos para \n", 
                       dim(covsGraf$df)[2], " variables",
                       " de las cuales,\n",
                       length(names(covsGraf$df)[lapply(covsGraf$df,class)!="factor"]), " es/son continua(s) y \n",
                       length(names(covsGraf$df)[lapply(covsGraf$df,class)=="factor"]), " es/son categórica(s) \n"
            ))
            
            remove_modal_spinner()
            
        }else{"Sin ningún dato cargado"}
        
    })
    
    ###### selector de valor x
    output$pickValGraf <- renderUI({
        
        if(!is.null(input$loadCovsGraf$datapath)){
            reset("grafSel")
            disabled(selectInput('pickCorrGrafUI', "Variable primaria", choices = c("---", names(covsGraf$df)[lapply(covsGraf$df,class)!="factor"]), width = "100%") ) 
        }else{ 
            disabled(selectInput('pickCorrGrafUI', "Variable primaria", choices = c("---"), width = "100%") )
        }
        
    })
    
    
    
    ###### selector de valor y
    
    
    output$pickValejeY <- renderUI({   ######### falta para la de densidad
        
        if( !is.null(input$loadCovsGraf$datapath) ){ 
            if(input$grafSel == "Gráfica de dispersión"){
                selectInput("ejeY", "Variable secundaria", choices=c("---", names(covsGraf$df)[lapply(covsGraf$df,class)!="factor"]), width = "100%")
            }else if(input$grafSel == "Diagrama de caja"){
                
                if(length(names(covsGraf$df)[lapply(covsGraf$df,class)=="factor"])>0){
                    selectInput("ejeY", "Variable secundaria", choices=c("---", names(covsGraf$df)[lapply(covsGraf$df,class)=="factor"]), width = "100%")
                }else{disabled(selectInput("ejeY", "Variable secundaria", choices=c("---"), width = "100%"))}
                
            }else{disabled(selectInput("ejeY", "Variable secundaria", choices=c("---"), width = "100%") )}
            
        }else{disabled(selectInput("ejeY", "Variable secundaria", choices=c("---"), width = "100%"))}
        
    })
    
    
    
    
    
    ###### desabilitador 
    observeEvent(input$grafSel,{
        
        if(input$grafSel == "---"){
            reset("pickCorrGrafUI")
            reset("ejeY")
            enable("ylab")
            disable("pickCorrGrafUI")
            reset("nbreaks")
            reset("addCheck")
        }else if( all(input$grafSel == "Histograma", !is.null(input$loadCovsGraf$datapath)) ){
            reset("pickCorrGrafUI")
            reset("ejeY")
            enable("ylab")
            enable("pickCorrGrafUI")
            reset("nbreaks")
            reset("addCheck")
        }else if( all(input$grafSel == "Gráfica de dispersión", !is.null(input$loadCovsGraf$datapath)) ){
            reset("ejeY")
            reset("pickCorrGrafUI")
            enable("ylab")
            enable("pickCorrGrafUI")
            reset("nbreaks")
            reset("addCheck")
        }else if( all(input$grafSel == "Diagrama de caja", !is.null(input$loadCovsGraf$datapath)) ){
            reset("pickCorrGrafUI")
            reset("ejeY")
            enable("ylab")
            enable("pickCorrGrafUI")
            reset("nbreaks")
            reset("addCheck")
        }else if( all(input$grafSel == "Gráfica de densidad", !is.null(input$loadCovsGraf$datapath))){
            reset("pickCorrGrafUI")
            reset("ejeY")
            enable("ylab")
            enable("pickCorrGrafUI")
            reset("nbreaks")
            reset("addCheck")
        }
        #else{enable("pickCorrGrafUI")}
        
    })
    
    observeEvent(input$DenGrafTipo,{
        
        if(input$DenGrafTipo=="Compuesta"){
            disable("ylab")
        }else{
            enable("ylab")
        }
    })
    
    
    
    
    ######## crea slider para xlim
    output$xlim <- renderUI({
        
        if( all(!is.null(input$loadCovsGraf$datapath) , input$pickCorrGrafUI!="---") ){
            
            if( all(input$grafSel == "Histograma", input$pickCorrGrafUI!="---") ){
                
                min.tmp<- round(min(covsGraf$df[,input$pickCorrGrafUI]),3)
                max.tmp<- round(max(covsGraf$df[,input$pickCorrGrafUI]),3)
                sliderInput("xlim","Rango eje x", min = min.tmp, max = max.tmp, value = c(min.tmp, max.tmp))
                
            }else if( all(input$grafSel == "Gráfica de dispersión", input$pickCorrGrafUI!="---") ){
                
                min.tmp<- round(min(covsGraf$df[,input$pickCorrGrafUI]),3)
                max.tmp<- round(max(covsGraf$df[,input$pickCorrGrafUI]),3)
                sliderInput("xlim","Rango eje x", min = min.tmp, max = max.tmp, value = c(min.tmp, max.tmp))
                
            }else if( all(input$grafSel == "Diagrama de caja", input$ejeY!="---") ){
                
                min.tmp<- 0
                max.tmp<- length(levels( covsGraf$df[,input$ejeY]) )+1
                sliderInput("xlim","Rango eje x", min = min.tmp, max = max.tmp, value = c(min.tmp, max.tmp), step=0.5)
                
            }else if( all(input$grafSel == "Gráfica de densidad", input$DenGrafTipo=="Densidad") ){
                
                d<-density(covsGraf$df[,input$pickCorrGrafUI])
                min.tmp<- round(min(d$x),3)
                max.tmp<- round(max(d$x),3)
                sliderInput("xlim","Rango eje x", min = min.tmp, max = max.tmp, value = c(min.tmp, max.tmp), step = 1)
                
            }else if( all(input$grafSel == "Gráfica de densidad", input$DenGrafTipo=="Histograma [f(x)]") ){
                
                min.tmp<- round(min(covsGraf$df[,input$pickCorrGrafUI]),3)
                max.tmp<- round(max(covsGraf$df[,input$pickCorrGrafUI]),3)
                sliderInput("xlim","Rango eje x", min = min.tmp, max = max.tmp, value = c(min.tmp, max.tmp))
                
            }
            
        }})
    
    
    ######## crea slider para ylim
    output$ylim <- renderUI({
        if( all(!is.null(input$loadCovsGraf$datapath), input$pickCorrGrafUI!="---" ) ){
            
            if( all(input$grafSel == "Histograma", class(h$fig)=="histogram") ){
                
                sliderInput("ylim","Rango eje y", min=0, max=max(h$fig$counts), value=c(0, max(h$fig$counts) ) )
                
            }else if( all(input$grafSel == "Gráfica de dispersión", input$ejeY!="---") ){
                
                if( class( try( round(min(covsGraf$df[,input$ejeY]),3), silent=T ) ) !="try-error" ){
                    min.tmp<- round(min(covsGraf$df[,input$ejeY]),3)
                    max.tmp<- round(max(covsGraf$df[,input$ejeY]),3)
                    sliderInput("ylim","Rango eje y", min = min.tmp, max = max.tmp, value = c(min.tmp, max.tmp))
                }
                
            }else if( all(input$grafSel == "Diagrama de caja", input$pickCorrGrafUI!="---") ){
                min.tmp<- round(min(covsGraf$df[,input$pickCorrGrafUI]),3)
                max.tmp<- round(max(covsGraf$df[,input$pickCorrGrafUI]),3)
                sliderInput("ylim","Rango eje y", min = min.tmp, max = max.tmp, value = c(min.tmp, max.tmp))
                
            }else if( all(input$grafSel == "Gráfica de densidad", input$DenGrafTipo=="Densidad") ){
                
                d<-density(covsGraf$df[,input$pickCorrGrafUI])
                min.tmp<- round(min(d$y),4)
                max.tmp<- round(max(d$y),4)
                sliderInput("ylim","Rango eje y", min = min.tmp, max = max.tmp, value = c(min.tmp, max.tmp))
                
            }else if( all(input$grafSel == "Gráfica de densidad", input$DenGrafTipo=="Histograma [f(x)]", class(h$fig)=="histogram") ){
                
                
                max.tmp<- max(h$fig$density)
                
                sliderInput("ylim","Rango eje y", min=0, max=max.tmp, value=c(0, max.tmp) )
                
            }
            
        }})
    
    
    
    ####### crea opciones adicionales 
    
    output$opcional1 <- renderUI({
        
        if(all(input$grafSel == "Histograma", input$pickCorrGrafUI!="---" )){
            numericInput("nbreaks", label="Numero de barras", value=25, min = 1, max=100, step=1, width = "25%")
        }else if( all(input$grafSel == "Gráfica de dispersión", input$pickCorrGrafUI!="---") ){
            numericInput("nbreaks", label="Tamaño de muestra [%]", value=1, min = 1, max=100, step=1, width = "75%")
            
        }else if( all(input$grafSel == "Gráfica de densidad", input$pickCorrGrafUI!="---", input$DenGrafTipo=="Histograma [f(x)]") ){
            numericInput("nbreaks", label="Numero de barras", value=25, min = 1, max=100, step=1, width = "75%")
            
        }else if( all(input$grafSel == "Diagrama de caja", input$pickCorrGrafUI!="---", input$ejeY!="---") ){
            numericInput("nbreaks", label="Margen eje x", value=5, min = 1, max=50, step=1, width = "75%")
            
        }
        
    })
    
    output$opcional2 <- renderUI({
        
        if( all(input$grafSel == "Gráfica de densidad")){
            disabled(selectInput("DenGrafTipo", label="Opciones de gráfica", selected="Densidad", choices = c("Densidad", "Histograma [f(x)]", "Compuesta")) )
        }
        
    })
    
    
    output$opcional3 <- renderUI({
        
        if( all(input$grafSel == "Gráfica de densidad", input$pickCorrGrafUI!="---", input$DenGrafTipo=="Histograma [f(x)]")){
            checkboxInput("addCheck", label="¿Función de densidad?", value=F) 
        }else if(all(input$grafSel == "Diagrama de caja", input$pickCorrGrafUI!="---")){
            checkboxInput("addCheck", label="¿Agregar media?", value=F) 
        }else if(all(input$grafSel == "Gráfica de dispersión", input$pickCorrGrafUI!="---")){
            checkboxInput("addCheck", label="¿Intervalos de confianza?", value=F) 
        }
        
    })
    
    
    ##### control de tipo de grafica de desidad
    observeEvent(input$pickCorrGrafUI, {
        
        if(input$pickCorrGrafUI!="---"){
            enable("DenGrafTipo")
        }
        
    })
    
    
    
    
    ########################## plotea (todas las graficas en 1 evento)
    output$grafica<- renderPlot({
        
        if( all(!is.null(input$loadCovsGraf$datapath) , input$pickCorrGrafUI!="---") ){
            
            
            width = "300px"
            height = "300px"
            res = 150
            
            if(input$grafSel == "Histograma"){
                if(!is.null(input$xlim)){ 
                    
                    if(!is.numeric(input$nbreaks)){nbreaks<-25}else{nbreaks<-input$nbreaks}
                    par( mar=c(5,5,5,2) )
                    
                    tmp<-hist(covsGraf$df[,input$pickCorrGrafUI], breaks=nbreaks ,col=input$Xcolor, main=input$main, xlab=input$xlab, ylab=input$ylab,
                              xlim=input$xlim, ylim=input$ylim, cex.main=input$mainCex, cex.lab=input$LabCex )
                    
                    
                    h$fig<- tmp
                }}
            
            else if(input$grafSel == "Gráfica de dispersión"){
                if( all(input$ejeY!="---", input$pickCorrGrafUI!="---") ){
                    
                    if(all(is.numeric(input$nbreaks), input$nbreaks<=100, input$nbreaks>=1 ) ){set.seed(input$nbreaks)
                        sam<-sample(1:dim(covsGraf$df)[1], dim(covsGraf$df)[1]*(input$nbreaks/100) )}
                    else{set.seed(1)
                        sam<-sample(1:dim(covsGraf$df)[1], dim(covsGraf$df)[1]*(0.010) )}
                    
                    par( mar=c(5,5,5,2) )
                    
                    plot(covsGraf$df[sam,input$pickCorrGrafUI], covsGraf$df[sam,input$ejeY], pch=19, col=input$Xcolor, main=input$main, xlab=input$xlab, ylab=input$ylab,
                         cex.main=input$mainCex, cex.lab=input$LabCex, xlim=input$xlim, ylim=input$ylim )
                    
                    
                    if( all(!is.null(input$addCheck), input$pickCorrGrafUI!="---", is.numeric(covsGraf$df[,input$ejeY]) ) ){   
                        if(input$addCheck){
                            
                            CI<-data.frame(x=covsGraf$df[sam,input$pickCorrGrafUI], y=covsGraf$df[sam,input$ejeY])
                            
                            m<-lm(y~ x, data=CI)
                            
                            tmp.new<-seq(min(CI$x), max(CI$x), length.out= dim(CI)[1])
                            
                            pred<-predict(m, newdata=data.frame(x=tmp.new), interval="confidence")
                            
                            abline(m)
                            lines(tmp.new, pred[,3], lty=2)
                            lines(tmp.new, pred[,2], lty=2)
                            polygon( c(rev(tmp.new), tmp.new), c( rev(pred[,3]), pred[,2] ), col=rgb(0, 0, 0,0.25) )
                            
                            
                        } }
                    
                    
                }}
            else if(input$grafSel == "Diagrama de caja"){
                
                if( all(input$ejeY!="---", input$pickCorrGrafUI!="---") ){
                    
                    if( any(!is.numeric(input$nbreaks), input$nbreaks>50) ){nbreaks<-5}else{nbreaks<-input$nbreaks}
                    
                    par( mar=c(nbreaks,5,5,2) )
                    
                    
                    boxplot(covsGraf$df[,input$pickCorrGrafUI] ~ covsGraf$df[,input$ejeY], col=input$Xcolor, main=input$main, xlab=input$xlab, ylab=input$ylab,
                            cex.main=input$mainCex, cex.lab=input$LabCex, las=2, xlim=input$xlim, ylim=input$ylim   )
                    
                    if( all(!is.null(input$addCheck), input$pickCorrGrafUI!="---", is.factor(covsGraf$df[,input$ejeY])) ){   
                        if(input$addCheck){
                            lines( 1:length(levels(covsGraf$df[,input$ejeY])), tapply(covsGraf$df[,input$pickCorrGrafUI], covsGraf$df[,input$ejeY], mean) , col="red")
                            points( 1:length(levels(covsGraf$df[,input$ejeY])), tapply(covsGraf$df[,input$pickCorrGrafUI], covsGraf$df[,input$ejeY], mean) , col="red", pch=19)
                        } }
                    
                    
                }else if(all(input$ejeY=="---", input$pickCorrGrafUI!="---") ){
                    par( mar=c(5,5,5,2) )
                    boxplot(covsGraf$df[,input$pickCorrGrafUI], col=input$Xcolor, main=input$main, xlab=input$xlab, ylab=input$ylab,
                            cex.main=input$mainCex, cex.lab=input$LabCex, ylim=input$ylim )
                    
                    if( !is.null(input$addCheck) ){   if(input$addCheck){abline(h=mean(covsGraf$df[,input$pickCorrGrafUI]), col="red" )}  }
                    
                    
                }}else if(input$grafSel == "Gráfica de densidad"){
                    
                    d<-density(covsGraf$df[,input$pickCorrGrafUI])
                    
                    if( any(input$DenGrafTipo=="Densidad", is.null(input$DenGrafTipo) ) ){
                        par( mar=c(5,5,5,2) )
                        plot(d, col=input$Xcolor, lwd=2, main=input$main, xlab=input$xlab, ylab=input$ylab, cex.main=input$mainCex, cex.lab=input$LabCex, 
                             xlim=input$xlim, ylim=input$ylim )
                        
                    }
                    else if(input$DenGrafTipo=="Compuesta"){
                        par(mfrow=c(2,1), mar=c(5,5,5,2))
                        plot(d, col=input$Xcolor, lwd=2, main=input$main, xlab="" ,ylab="Densidad", cex.main=input$mainCex, cex.lab=input$LabCex)
                        hist(covsGraf$df[,input$pickCorrGrafUI], main="", col=input$Xcolor, xlab=input$xlab, ylab="Frecuencia", cex.main=input$mainCex, cex.lab=input$LabCex)
                        
                    }
                    else if(input$DenGrafTipo=="Histograma [f(x)]"){
                        
                        if(!is.numeric(input$nbreaks)){nbreaks<-25}else{nbreaks<-input$nbreaks}
                        
                        d<-density(covsGraf$df[,input$pickCorrGrafUI])
                        par( mar=c(5,5,5,2) )
                        
                        tmp<-hist(covsGraf$df[,input$pickCorrGrafUI], freq=F, breaks=nbreaks, col=input$Xcolor, main=input$main, xlab=input$xlab, ylab=input$ylab, 
                                  cex.main=input$mainCex, cex.lab=input$LabCex, xlim=input$xlim, ylim=input$ylim )
                        
                        h$fig<- tmp
                        
                        if( !is.null(input$addCheck) ){
                            if(input$addCheck){
                                lines(d, col="red", lwd=2)
                            }
                        }
                        
                    }
                    
                }
            
            
        }
        
    })
    
    
    
    toListenGraf <- reactive({
        list(input$RutaGraf,input$fileGraf,input$pickCorrGrafUI,input$ejeY)
    })
    
    observeEvent(toListenGraf(), {
        
        cond.tmp<-all(input$RutaGraf != "", input$fileGraf!="", input$pickCorrGrafUI!="---" )
        
        if( all(cond.tmp, input$grafSel == "Gráfica de dispersión", input$ejeY!="---") ){
            enable("runGraf")
        }else if(all(cond.tmp, input$grafSel == "Gráfica de dispersión", input$ejeY=="---")){
            disable("runGraf")
        }else if(cond.tmp){
            enable("runGraf")
        }else{
            disable("runGraf")
        }
        
        
    })
    
    
    
    
    
    observeEvent(input$runGraf, {
        
        
        if(!dir.exists(input$RutaGraf)){
            
            showModal( modalDialog(h2("¡Error!"),
                                   helpText("El directorio de salida no existe o no se puede tener acceso") ,
                                   footer=modalButton("Cerrar"), size="m", ) )
            
        }else{
            
            
            png(paste0(input$RutaGraf, "/", input$fileGraf, ".png"), width = 960*input$anchoGraf , height = 960*input$altoGraf, pointsize=input$PxGraf, res=input$ResGraf)
            
            
            
            if( all(!is.null(input$loadCovsGraf$datapath) , input$pickCorrGrafUI!="---") ){
                
                
                width = "300px"
                height = "300px"
                res = 150
                
                if(input$grafSel == "Histograma"){
                    if(!is.null(input$xlim)){ 
                        
                        if(!is.numeric(input$nbreaks)){nbreaks<-25}else{nbreaks<-input$nbreaks}
                        par( mar=c(5,5,5,2) )
                        tmp<-hist(covsGraf$df[,input$pickCorrGrafUI], breaks=nbreaks ,col=input$Xcolor, main=input$main, xlab=input$xlab, ylab=input$ylab,
                                  xlim=input$xlim, ylim=input$ylim, cex.main=input$mainCex, cex.lab=input$LabCex )
                        
                        h$fig<- tmp
                    }}
                
                else if(input$grafSel == "Gráfica de dispersión"){
                    if( all(input$ejeY!="---", input$pickCorrGrafUI!="---") ){
                        
                        if(all(is.numeric(input$nbreaks), input$nbreaks<=100, input$nbreaks>=1 ) ){set.seed(input$nbreaks)
                            sam<-sample(1:dim(covsGraf$df)[1], dim(covsGraf$df)[1]*(input$nbreaks/100) )}
                        else{set.seed(1)
                            sam<-sample(1:dim(covsGraf$df)[1], dim(covsGraf$df)[1]*(0.010) )}
                        
                        par( mar=c(5,5,5,2) )
                        plot(covsGraf$df[sam,input$pickCorrGrafUI], covsGraf$df[sam,input$ejeY], pch=19, col=input$Xcolor, main=input$main, xlab=input$xlab, ylab=input$ylab,
                             cex.main=input$mainCex, cex.lab=input$LabCex, xlim=input$xlim, ylim=input$ylim )
                        
                        
                        if( all(!is.null(input$addCheck), input$pickCorrGrafUI!="---", is.numeric(covsGraf$df[,input$ejeY]) ) ){   
                            if(input$addCheck){
                                
                                CI<-data.frame(x=covsGraf$df[sam,input$pickCorrGrafUI], y=covsGraf$df[sam,input$ejeY])
                                
                                m<-lm(y~ x, data=CI)
                                
                                tmp.new<-seq(min(CI$x), max(CI$x), length.out= dim(CI)[1])
                                
                                pred<-predict(m, newdata=data.frame(x=tmp.new), interval="confidence")
                                
                                abline(m)
                                lines(tmp.new, pred[,3], lty=2)
                                lines(tmp.new, pred[,2], lty=2)
                                polygon( c(rev(tmp.new), tmp.new), c( rev(pred[,3]), pred[,2] ), col=rgb(0, 0, 0,0.25) )
                                
                                
                            } }
                        
                        
                    }}
                else if(input$grafSel == "Diagrama de caja"){
                    
                    if( all(input$ejeY!="---", input$pickCorrGrafUI!="---") ){
                        
                        if( any(!is.numeric(input$nbreaks), input$nbreaks>50) ){nbreaks<-5}else{nbreaks<-input$nbreaks}
                        
                        par( mar=c(nbreaks,5,5,2) )
                        
                        
                        boxplot(covsGraf$df[,input$pickCorrGrafUI] ~ covsGraf$df[,input$ejeY], col=input$Xcolor, main=input$main, xlab=input$xlab, ylab=input$ylab,
                                cex.main=input$mainCex, cex.lab=input$LabCex, las=2, xlim=input$xlim, ylim=input$ylim   )
                        
                        if( all(!is.null(input$addCheck), input$pickCorrGrafUI!="---", is.factor(covsGraf$df[,input$ejeY])) ){   
                            if(input$addCheck){
                                lines( 1:length(levels(covsGraf$df[,input$ejeY])), tapply(covsGraf$df[,input$pickCorrGrafUI], covsGraf$df[,input$ejeY], mean) , col="red")
                                points( 1:length(levels(covsGraf$df[,input$ejeY])), tapply(covsGraf$df[,input$pickCorrGrafUI], covsGraf$df[,input$ejeY], mean) , col="red", pch=19)
                            } }
                        
                        
                    }else if(all(input$ejeY=="---", input$pickCorrGrafUI!="---") ){
                        par( mar=c(5,5,5,2) )
                        boxplot(covsGraf$df[,input$pickCorrGrafUI], col=input$Xcolor, main=input$main, xlab=input$xlab, ylab=input$ylab,
                                cex.main=input$mainCex, cex.lab=input$LabCex, ylim=input$ylim )
                        
                        if( !is.null(input$addCheck) ){   if(input$addCheck){abline(h=mean(covsGraf$df[,input$pickCorrGrafUI]), col="red" )}  }
                        
                        
                    }}else if(input$grafSel == "Gráfica de densidad"){
                        
                        d<-density(covsGraf$df[,input$pickCorrGrafUI])
                        
                        if( any(input$DenGrafTipo=="Densidad", is.null(input$DenGrafTipo) ) ){
                            par( mar=c(5,5,5,2) )
                            plot(d, col=input$Xcolor, lwd=2, main=input$main, xlab=input$xlab, ylab=input$ylab, cex.main=input$mainCex, cex.lab=input$LabCex, 
                                 xlim=input$xlim, ylim=input$ylim )
                            
                        }
                        else if(input$DenGrafTipo=="Compuesta"){
                            par(mfrow=c(2,1), mar=c(5,5,5,2))
                            plot(d, col=input$Xcolor, lwd=2, main=input$main, xlab="" ,ylab="Densidad", cex.main=input$mainCex, cex.lab=input$LabCex)
                            hist(covsGraf$df[,input$pickCorrGrafUI], main="", col=input$Xcolor, xlab=input$xlab, ylab="Frecuencia", cex.main=input$mainCex, cex.lab=input$LabCex)
                            
                        }
                        else if(input$DenGrafTipo=="Histograma [f(x)]"){
                            
                            if(!is.numeric(input$nbreaks)){nbreaks<-25}else{nbreaks<-input$nbreaks}
                            
                            d<-density(covsGraf$df[,input$pickCorrGrafUI])
                            par( mar=c(5,5,5,2) )
                            tmp<-hist(covsGraf$df[,input$pickCorrGrafUI], freq=F, breaks=nbreaks, col=input$Xcolor, main=input$main, xlab=input$xlab, ylab=input$ylab, 
                                      cex.main=input$mainCex, cex.lab=input$LabCex, xlim=input$xlim, ylim=input$ylim )
                            
                            h$fig<- tmp
                            
                            if( !is.null(input$addCheck) ){
                                if(input$addCheck){
                                    lines(d, col="red", lwd=2)
                                }
                            }
                            
                        }
                        
                    }
                
                
            }
            
            
            dev.off()
            
        }
        
    })
    
    
    
    #####  
    
    ##### server graficas campo ####  
    
    
    numCovs<-reactiveValues(df=data.frame(Variable=character(), Value=numeric()) )
    numCovs.geo<-reactiveValues(map="")
    
    shps.tmp<-reactiveValues(shp="")
    shps.qry<-reactiveValues(dat=data.frame(file="", etiqueta="", stringsAsFactors = F) )
    
    
    
    output$NumCovsUI<- renderUI({
        
        #print(dim(numCovs$df))
        
        if( !is.null( input$covsPrep$datapath ) ){
            
            if(dim(numCovs$df)[2]==0){helpText("El archivo cargado no contiene variables de tipo numérico")
            }else{
                shps.tmp<-reactiveValues(shp="")
                checkboxGroupInput("checkGroupCovs1", label=NULL, choices = names(numCovs$df)[c(-1,-2, -dim(numCovs$df)[2] )], inline = F, selected=NULL)  #names(numCovs$df)
            }
            
        }else{helpText("Sin ningún dato cargado")}
        
    })
    
    
    observeEvent(input$covsPrep,{
        
        if( !is.null( input$covsPrep$datapath ) ){
            show_modal_spinner()
            tmpCovsPrep<- readRDS(input$covsPrep$datapath)
            
            numCovs$df<-tmpCovsPrep[,lapply(tmpCovsPrep,class)!="factor"]
            
            shps.tmp<-reactiveValues(shp="")
            updateCheckboxGroupInput(session = getDefaultReactiveDomain(), "checkGroupCovs1", selected =NULL )
            
            if( dim(numCovs$df)[2]>0 ){
                numCovs$df$control<-rownames(numCovs$df)
                numCovs.geo$map<-numCovs$df
                coordinates(numCovs.geo$map)<-~x+y
                gridded(numCovs.geo$map)<-T
                enable("checkGroup2")
                remove_modal_spinner()
            }else{
                showModal( modalDialog(h2("¡Error!"), helpText("El archivo de covariables introducido no contiene variables de tipo continuo") ,footer=modalButton("Cerrar"), size="m", ))
                
            }
            
        }
        
    })
    
    
    
    
    output$checkBoxG<- renderUI({
        
        if(input$DirCheck != ""){
            if( dir.exists(input$DirCheck) ){
                wd<-input$DirCheck  
                dirs<-list.dirs(wd, full.names = F, recursive = F)
                
                if(!is.null( input$covsPrep$datapath ) ){
                    selectInput("checkGroup2", label = NULL, choices = c("---",dirs), selected = "---") 
                    
                }else{
                    disabled( selectInput("checkGroup2", label = NULL, choices = c("---",dirs), selected = "---") )
                }
            }else{
                helpText("Error: El direcorio raíz no existe o no se puede tener acceso.")
            }
        }else{
            helpText("Seleccione un directorio raíz")
        }
    })
    
    
    
    
    
    
    output$dirShps<- renderUI({
        
        if( all( !is.null(input$checkGroup2), input$checkGroup2!="---", input$DirCheck!="", dir.exists(input$DirCheck) ) ){
            #shps<-list.files(path=paste0(input$DirCheck,"/", input$checkGroup2), pattern = ".shp$", full.names = F, recursive=F)
            #checkboxGroupInput("checkGroupSHPs", label=NULL, choices = shps, inline = F, selected = shps )  
            
            
            checkboxGroupInput("checkGroupSHPs", label=NULL, choices = shps.qry$dat$file  , inline = F, selected = shps.qry$dat$file ) 
        }else{helpText("Seleccione una carpeta")}
    })
    
    
    
    
    observeEvent(input$DirCheck,{
        shps.tmp$shp=""
        reset("checkGroupSHPs")
        reset("checkGroup2")
        reset("checkGroupCovs1")
        output$plotPrep<- renderPlot({plot.new()})
    })
    
    
    
    observeEvent(input$checkGroup2, {
        
        shps.qry$dat <- data.frame(file="", etiqueta="", stringsAsFactors = F) ### inicaliza objeto
        
        
        
        if(input$checkGroup2!="---"){
            show_modal_spinner()
            
            shps2<-list.files(path=paste0(input$DirCheck,"/", input$checkGroup2), pattern = ".shp$", full.names = F, recursive=F)
            
            shps.tmp$shp<-readOGR(paste0(input$DirCheck,"/", input$checkGroup2,"/", shps2[1]), verbose=F )
            
            #print(shps2[1])
            
            if( ("Etiqueta" %in% names(shps.tmp$shp) ) ){
                shps.tmp$shp<-shps.tmp$shp[,c("Etiqueta")]
                
                shps.qry$dat$file[1]<-shps2[1]
                shps.qry$dat$etiqueta[1]<- unique( as.character(shps.tmp$shp$Etiqueta) )
                
                
            }else{
                
                shps.tmp$shp@data$Etiqueta<-"Error"
                shps.tmp$shp<-shps.tmp$shp[,c("Etiqueta")]
                
                shps.qry$dat$file[1]<-"Error"
                
                
            }
            
            
            if(length(shps2)>1){
                for(i in 2:length(shps2)){
                    
                    #  print(shps2[i])
                    tmpSHP <- readOGR(paste0(input$DirCheck,"/", input$checkGroup2,"/", shps2[i]), verbose=F )
                    
                    if( ("Etiqueta" %in% names(tmpSHP) ) ){
                        
                        tmpSHP<-tmpSHP[,c("Etiqueta")]
                        
                        shps.tmp$shp<-rbind(shps.tmp$shp, tmpSHP)
                        
                        shps.qry$dat <- rbind( shps.qry$dat, data.frame(file=shps2[i], etiqueta=unique(tmpSHP$Etiqueta), stringsAsFactors = F ) )
                        
                    }
                    
                    
                }}
            
            shps.tmp$shp<-shps.tmp$shp[which(shps.tmp$shp$Etiqueta != "Error"),]
            
            shps.qry$dat<-shps.qry$dat[which(shps.qry$dat$file != "Error"),]
            
            
            if( class(shps.tmp$shp$Etiqueta)!="factor"){shps.tmp$shp$Etiqueta<-as.factor(shps.tmp$shp$Etiqueta)}
            
            
            # print(levels(shps.tmp$shp$Etiqueta))
            
            crs(shps.tmp$shp)<-""
            crs(numCovs.geo$map)<-""
            ov<- over(shps.tmp$shp, numCovs.geo$map)
            
            shps.tmp$shp@data<-cbind(shps.tmp$shp@data, ov)
            
            remove_modal_spinner()
        }
        
        
        
    })  
    
    
    toListenGrafsPrep<- reactive({
        list(input$checkGroupSHPs,input$checkGroupCovs1,input$rmDup, shps.tmp$shp)
    })
    
    
    observeEvent(toListenGrafsPrep() ,{ #input$checkGroupSHPs 
        
        #print(class(shps.tmp$shp))
        Conts<- numCovs.geo$map
        # str(Conts)
        
        # print(input$checkGroup2!="---")
        
        if( all(class(shps.tmp$shp)=="SpatialPointsDataFrame", input$checkGroup2!="---") ){
            
            
            #   print(dim(shps.tmp$shp))
            
            if(input$rmDup){shps.tmp$shp2<-shps.tmp$shp[!duplicated(shps.tmp$shp$control),]}else{shps.tmp$shp2<-shps.tmp$shp}  
            
            sam <- shps.qry$dat[shps.qry$dat$file %in% (input$checkGroupSHPs),]
            
            cols <- randomColor(length(levels(shps.tmp$shp2$Etiqueta)), hue = c(" ", "random", "red", "orange", "yellow",
                                                                                "green", "blue", "purple", "pink", "monochrome"), luminosity = c(" ","random", "light", "bright", "dark"))
            
            
            #print(head(shps.tmp$shp2@data))
            
            shp.selected <-  shps.tmp$shp2[ shps.tmp$shp2@data$Etiqueta %in% sam$etiqueta , ] #input$checkGroupCovs1
            
            shps.tmp$shp3 <- shps.tmp$shp[ shps.tmp$shp@data$Etiqueta %in% sam$etiqueta , ]
            
            
            
            # str(shp.selected)
            
            nr<-length(input$checkGroupCovs1)
            nr2<-nr*2
            
            ly<-c(unlist(lapply(c(1:nr2)[1:nr2 %% 2 !=0], rep, times=3)),
                  unlist(lapply(c(1:nr2)[1:nr2 %% 2 ==0], rep, times=6)))
            
            ly<-ly[order(ly)]
            
            
            # print(input$checkGroupCovs1)
            # print(nr)
            
            #print(str(shp.selected@data))
            
            output$plotPrep<- renderPlot({ 
                
                ly2<- layout( matrix(ly,nc=3, byrow=T))  
                #  par(mfrow=c(nr*2, 1), mar=c(5,5,5,1))
                
                
                
                if( any(nr==0, length(input$checkGroupSHPs)==0) ){
                    plot.new()
                    
                }else{
                    
                    for(i in 1:nr){ # dim(shp.selected)[2]
                        
                        
                        d<-max(density(Conts@data[,input$checkGroupCovs1[i]],na.rm=T)$y)
                        d1<-max(density(shp.selected@data[,input$checkGroupCovs1[i]],na.rm=T)$y)
                        d1<-d1+abs(d1*0.15)
                        
                        d2<-min(min(density(Conts@data[,input$checkGroupCovs1[i]],na.rm=T)$x),min(density(shp.selected@data[,input$checkGroupCovs1[i]],na.rm=T)$x))
                        d2<-d2-abs(d2*0.15)
                        
                        d3<-max(max(density(Conts@data[,input$checkGroupCovs1[i]],na.rm=T)$x),max(density(shp.selected@data[,input$checkGroupCovs1[i]],na.rm=T)$x))
                        d3<-d3+abs(d3*0.15)
                        
                        
                        
                        plot(density(Conts@data[,input$checkGroupCovs1[i]],na.rm=T), col="red", lwd=1.5, ylim=c(0,max(d,d1)), main="", cex.axis=1.5, cex.lab=1.5, xlab="" )
                        lines(density(shp.selected@data[,input$checkGroupCovs1[i]],na.rm=T), lwd=1.5, col="black")
                        legend("top", legend=c("Nacional","Todas las muestras"), col=c("red","black"), lwd=c(2.5,2.5), bty="n",cex=1.5 )
                        
                        legend("topright",cex=1.5,y.intersp=1,ncol = 2,inset=0,
                               paste(
                                   c(names(summary(shp.selected@data[,input$checkGroupCovs1[i]]))),
                                   c(round(summary(shp.selected@data[,input$checkGroupCovs1[i]]),3) ), sep=" "),
                               bty="n", text.col=c("grey65","blue","green","brown4","red","grey65","mediumorchid1")
                        )
                        
                        
                        
                        axis(3,lwd=2,line=3.5, at= range(Conts@data[,input$checkGroupCovs1[i]]), labels=FALSE, ann=FALSE)
                        mtext(3,text=input$checkGroupCovs1[i],line=2)
                        
                        abline(v=round(summary(shp.selected@data[,input$checkGroupCovs1[i]]),3)[1], col="grey65", lty=2, lwd=2)
                        abline(v=round(summary(shp.selected@data[,input$checkGroupCovs1[i]]),3)[2], col="blue", lty=2, lwd=2)
                        abline(v=round(summary(shp.selected@data[,input$checkGroupCovs1[i]]),3)[3], col="green", lty=2, lwd=2)
                        abline(v=round(summary(shp.selected@data[,input$checkGroupCovs1[i]]),3)[4], col="brown4", lty=2, lwd=2)
                        abline(v=round(summary(shp.selected@data[,input$checkGroupCovs1[i]]),3)[5], col="red", lty=2, lwd=2)
                        abline(v=round(summary(shp.selected@data[,input$checkGroupCovs1[i]]),3)[6], col="grey65", lty=2, lwd=2)
                        
                        
                        
                        
                        shp.selected@data$Etiqueta<-as.factor(as.character(shp.selected@data$Etiqueta))
                        
                        # shps.tmp$shp3@data$Etiqueta<-as.factor(as.character(shps.tmp$shp3@data$Etiqueta))
                        shps.tmp$shp3@data<- shps.tmp$shp3@data[which(shps.tmp$shp3@data$Etiqueta %in% levels(shp.selected@data$Etiqueta)),]
                        
                        shps.tmp$shp3@data$Etiqueta<-as.factor(as.character(shps.tmp$shp3@data$Etiqueta))
                        
                        
                        #  print(levels(shp.selected@data$Etiqueta))
                        # print(levels(shps.tmp$shp3@data$Etiqueta))
                        
                        tmp.1 <- shp.selected@data[which(shp.selected@data$Etiqueta == levels(shp.selected@data$Etiqueta)[1]),]
                        plot(density(tmp.1[,input$checkGroupCovs1[i]],na.rm=T), lwd=1.5, ylim=c(0,max(d,d1)), xlim=c(d2,d3), xlab=input$checkGroupCovs1[i],
                             main="Densidad por especie", col=cols[1])
                        
                        
                        
                        if(length(levels(shp.selected@data$Etiqueta))>1){
                            for(esp in 2:length(levels(shp.selected@data$Etiqueta))){
                                
                                tmp.1<-shp.selected@data[which(shp.selected@data$Etiqueta == levels(shp.selected@data$Etiqueta)[esp]),]
                                #  str(tmp.1)
                                
                                # print(dim(tmp.1))
                                
                                if(dim(tmp.1)[1]>1){
                                    lines(density(tmp.1[,input$checkGroupCovs1[i]],na.rm=T), lwd=1.5, ylim=c(0,max(d,d1)), xlim=c(d2,d3), xlab=input$checkGroupCovs1[i],
                                          col=cols[esp])
                                }### fin if numero de pts en shp
                                
                            }### fin for esp
                        }### fin if numero de shps
                        
                        
                        legend("topright", legend=
                                   paste(levels(shp.selected@data$Etiqueta), tapply(shps.tmp$shp3@data$Etiqueta, shps.tmp$shp3@data$Etiqueta, length ), 
                                         tapply(shp.selected@data$Etiqueta, shp.selected@data$Etiqueta,length), sep=" n:"), col=cols, lty=1, bty="n", cex=1.5, lwd=2)
                        
                        
                        
                    }
                }#fin else
                
                
                
                
                
            }, height= function(x){if(nr==0){return(750)}else{return(750*nr)} })
            
        }else{ output$plotPrep<- renderPlot({ plot.new() }) } 
        
    })  
    
    ###### crea canvas para plot
    output$plotCanvas<- renderUI({
        
        nr<-length(input$checkGroupCovs1)
        
        if(nr==0){
            plotOutput("plotPrep",  height = paste0(750, "px"), width = "100%" )
            
        }else{
            plotOutput("plotPrep",  height = paste0(750*nr, "px"), width = "100%" )
            
        }
        
    })
    
    
    
    
    
    
    
    #####  
    
    ###### server graficas y estadisticas ####  
    
    CovsExp<-reactiveValues(df=data.frame(Covariable=character(), Value=numeric()) )
    CovsExp.geo<-reactiveValues(map="")
    
    
    #### crea drop de las carpetas
    output$checkBoxDirExp<- renderUI({
        
        if(input$DirCheckExp != ""){
            if(dir.exists(input$DirCheckExp)){
                wd<-input$DirCheckExp   
                dirs<-list.dirs(wd, full.names = F, recursive = F)
                
                if(!is.null( input$covsPrepExp$datapath ) ){
                    selectInput("checkGroup2Exp", label = NULL, choices = c("Todas las carpetas",dirs), selected = "Todas las carpetas") 
                    
                }else{
                    disabled( selectInput("checkGroup2Exp", label = NULL, choices = c("Todas las carpetas",dirs), selected = "Todas las carpetas") )
                }
                
            }else{
                helpText("Error: El direcorio raíz no existe o no se puede tener acceso.")
            }
        }else{
            helpText("Seleccione un directorio raíz")
        }
        
        
    })  
    
    
    ################# tabla 1
    react_valsExp <- reactiveValues(
        df = data.frame(ID=numeric(), Covariable=character(), Alias=character(), Tipo=character(), stringsAsFactors=F ),
        dt_row = NULL,
        add_or_edit = NULL,
        edit_button = NULL,
        keep_track_id = nrow(df) + 1
    )
    
    ##### crea tabla con el contenido reactivo de react_vals
    output$covsExploracionIN <- renderDataTable(
        {
            isolate(react_valsExp$df)
        },
        escape = F, 
        rownames = FALSE,  caption = tags$caption(style="caption-side: top; text-align: left; margin: 8px 5;","Tabla 1. Covariables de entrada"),
        options = list(processing = FALSE, ordering=T, paging = T,  language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'), pageLength = 5,
                       headerCallback = JS(
                           "function( thead, data, start, end, display ) {
      $(thead).closest('thead').find('th').css('background-color', '#7A1F1A');
              }"),
      initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'color': 'white'});",
          "}"
      ), autoWidth = F, scrollX=T
      
        ),
      editable = list(target = "cell", disable = list(columns =c(0,1,3)))
    )
    
    #### proxy de tabla para el manejo interno 
    proxyExpIN <- dataTableProxy("covsExploracionIN") 
    observe({
        replaceData(proxyExpIN, react_valsExp$df, resetPaging = FALSE, rownames = FALSE)
    })
    
    
    observeEvent(input$covsPrepExp,{
        
        if( !is.null( input$covsPrepExp$datapath ) ){
            show_modal_spinner()
            CovsExp$df <- readRDS(input$covsPrepExp$datapath)
            
            
            
            react_valsExp$df = data.frame(ID=1:(dim(CovsExp$df)[2]-2) ,Covariable=names(CovsExp$df)[-c(1,2)], Alias=names(CovsExp$df)[-c(1,2)], Tipo= unlist(lapply(CovsExp$df,class))[-c(1,2)], stringsAsFactors=F )
            # print(names(CovsExp$df))            
            
            react_valsExpT2$df <- data.frame(ID=numeric(), Covariable=character(), Alias=character(), Tipo=character(), stringsAsFactors=F )
            
            # if( dim(CovsExp$df)[2]>0 ){
            CovsExp$df$control<-rownames(CovsExp$df)
            CovsExp.geo$map<-CovsExp$df
            coordinates(CovsExp.geo$map)<-~x+y
            gridded(CovsExp.geo$map)<-T
            #  enable("checkGroup2")
            remove_modal_spinner()
            # }else{
            #     showModal( modalDialog(h2("¡Error!"), helpText("El archivo de covariables introducido no contiene variables de tipo continuo") ,footer=modalButton("Cerrar"), size="m", ))
            #     
            # }
            
        }
        
    })
    
    
    
    ######## evento para la actualizacion de edicion de celda
    observeEvent(input$covsExploracionIN_cell_edit,{
        ind = input$covsExploracionIN_cell_edit
        #str(info)
        i = ind$row
        j = ind$col+1
        z = ind$value
        #print(paste0(i,"-",j,"-",z))
        react_valsExp$df[i, j] <- isolate(z)
    })
    
    #### evento elimina renglon de tabla principal
    observeEvent(input$RmVarExp,{
        if(!is.null(input$covsExploracionIN_rows_selected)){
            
            elim<-react_valsExp$df[input$covsExploracionIN_rows_selected, ]
            react_valsExp$df <- react_valsExp$df[-input$covsExploracionIN_rows_selected, ]
            
            react_valsExpT2$df<- rbind(react_valsExpT2$df, elim)
            
        }
        reset("covsExploracionIN_rows_selected")
    })
    
    
    #### evento regresa renglon a tabla principal
    observeEvent(input$addVarExp,{
        if(!is.null(input$covsExploracionOUT_rows_selected)){
            
            elim<-react_valsExpT2$df[input$covsExploracionOUT_rows_selected, ]
            react_valsExpT2$df <- react_valsExpT2$df[-input$covsExploracionOUT_rows_selected, ]
            react_valsExp$df<- rbind(react_valsExp$df, elim)
        }
        reset("covsExploracionOUT_rows_selected")
    })
    
    
    ###### disabler
    toListenExploration<- reactive({
        list(input$covsExploracionIN_rows_selected, input$covsExploracionOUT_rows_selected )
    })
    
    observeEvent(toListenExploration() ,{
        
        #condDisablerExp<-any(!is.null(input$covsExploracionIN_rows_selected), !is.null(input$covsExploracionOUT_rows_selected))
        
        if(!is.null(input$covsExploracionIN_rows_selected)){
            enable("RmVarExp")
        }else{disable("RmVarExp")}
        
        if(!is.null(input$covsExploracionOUT_rows_selected)){
            enable("addVarExp")
        }else{disable("addVarExp")}
        
        
    })
    
    
    ###### disabler 2
    toListenExploration2<- reactive({
        list(input$DirCheckExp , react_valsExp$df )
    })
    
    observeEvent(toListenExploration2(), {
        
        if( all(dim(react_valsExp$df)[1]!=0, input$DirCheckExp!="", dir.exists(input$DirCheckExp) ) ){
            enable("runBatchStats")
            if( any(react_valsExp$df$Tipo %in% c("numeric", "integer") ) ){
                enable("runBatchGraf")
            }else{disable("runBatchGraf")}
            
        }else{
            disable("runBatchGraf")
            disable("runBatchStats")
        }
        
        
    })
    
    
    ######################   tabla 2
    
    react_valsExpT2 <- reactiveValues(
        df = data.frame(ID=numeric(), Covariable=character(), Alias=character(), Tipo=character(), stringsAsFactors=F ),
        dt_row = NULL,
        add_or_edit = NULL,
        edit_button = NULL,
        keep_track_id = nrow(df) + 1
    )
    
    output$covsExploracionOUT <- renderDataTable(
        {
            isolate(react_valsExpT2$df)
        },
        escape = F, 
        rownames = FALSE,  caption = tags$caption(style="caption-side: top; text-align: left; margin: 8px 5;","Tabla 2. Covariables sin usar"),
        options = list(processing = FALSE, ordering=T, paging = T,  language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'), pageLength = 5,
                       headerCallback = JS(
                           "function( thead, data, start, end, display ) {
      $(thead).closest('thead').find('th').css('background-color', '#7A1F1A');
              }"),
      initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'color': 'white'});",
          "}"
      ), autoWidth = F, scrollX=T
      
        ),
      editable = F
    )
    
    #### proxy de tabla para el manejo interno 
    proxyExpOUT <- dataTableProxy("covsExploracionOUT") 
    observe({
        replaceData(proxyExpOUT, react_valsExpT2$df, resetPaging = FALSE, rownames = FALSE)
    })
    
    
    
    
    ######## escribe los archivos de las estadísticas
    observeEvent(input$runBatchStats,{
        
        
        
        
        #print(str(CovsExp.geo$map))
        #print(input$checkGroup2Exp)
        
        if(input$checkGroup2Exp == "Todas las carpetas"){
            ipb<-0
            #print("TEST")
            withProgress(message = "Compilando", value = 0, {
                show_modal_spinner()
                
                dirs<-list.dirs(input$DirCheckExp, full.names = F, recursive = F)     
                
                
                for(ndirs in 1:length(dirs)){
                    
                    ipb<-ipb+1
                    incProgress(1/length(dirs), detail = paste("Carpeta: ", dirs[ndirs]))
                    
                    
                    
                    wd<- paste0(input$DirCheckExp, "/", dirs[ndirs] )
                    #print(wd)
                    
                    ### crea carpetas de salida
                    outDirs<- c(paste0(wd, "/", "Resultados"), 
                                paste0(wd, "/", "Resultados/Estadisticas"), 
                                paste0(wd, "/", "Resultados/Graficas"), 
                                paste0(wd, "/", "Resultados/Modelos") )
                    
                    sapply(outDirs[!sapply(outDirs, dir.exists)], dir.create)
                    
                    
                    f<-list.files(wd, pattern=".shp$", full.names=F, recursive=F)
                    #print(f)
                    
                    
                    shps.tmp<-readOGR(paste0(wd,"/", f[1]), verbose=F )
                    
                    #print("Etiqueta" %in% names(shps.tmp))
                    
                    if( ("Etiqueta" %in% names(shps.tmp) ) ){
                        shps.tmp<-shps.tmp[,c("Etiqueta")]
                        
                    }else{
                        
                        shps.tmp@data$Etiqueta<-paste0("Etiqueta no determinada en ", f[1] )
                        shps.tmp<-shps.tmp[,c("Etiqueta")]
                        
                    }
                    
                    
                    if(length(f)>1){
                        for(i in 2:length(f)){
                            
                            # print(f[i])
                            tmpSHP <- readOGR(paste0(wd,"/", f[i]), verbose=F )
                            
                            # print("Etiqueta" %in% names(tmpSHP))
                            
                            
                            if( ("Etiqueta" %in% names(tmpSHP) ) ){
                                
                                tmpSHP<-tmpSHP[,c("Etiqueta")]
                                shps.tmp<-rbind(shps.tmp, tmpSHP)
                                
                            }else{
                                
                                tmpSHP@data$Etiqueta<-paste0("Etiqueta no determinada en ", f[i] )
                                tmpSHP<-tmpSHP[,c("Etiqueta")]
                                shps.tmp<-rbind(shps.tmp, tmpSHP)
                            }  
                            
                        }}
                    
                    crs(shps.tmp)<-""
                    crs(CovsExp.geo$map)<-""
                    
                    ov<- over(shps.tmp, CovsExp.geo$map)
                    
                    shps.tmp@data<-cbind(shps.tmp@data, ov)
                    
                    if(input$rmDupExp){shps.tmp<-shps.tmp[!duplicated(shps.tmp@data$control),]}
                    
                    #print( dim(shps.tmp@data ))
                    #print( summary(shps.tmp@data ))
                    # print( react_valsExp$df )
                    
                    CatVars <- react_valsExp$df[ which(react_valsExp$df$Tipo=="factor"), ] 
                    ConVars <- react_valsExp$df[ which(react_valsExp$df$Tipo!="factor"), ] 
                    
                    #print(ConVars)
                    #print(CatVars)
                    
                    if( class(shps.tmp$Etiqueta)!="factor"){shps.tmp$Etiqueta<-as.factor(shps.tmp$Etiqueta)}
                    
                    ######## stats vars categóricas
                    #print(dim(CatVars)[1])
                    
                    if(dim(CatVars)[1]>0){
                        for(var in 1:(dim(CatVars)[1]) ){
                            
                            res<-data.frame(Especie=levels(shps.tmp$Etiqueta))
                            res[  levels(   shps.tmp@data[,CatVars$Covariable[var]]   )  ]<-NA
                            
                            #  print(CatVars$Covariable[var])
                            # print( res )
                            # print(names(shps.tmp@data))
                            
                            for(e in 1:length(levels(shps.tmp$Etiqueta))){
                                
                                tmp.1<-shps.tmp@data[which(shps.tmp@data$Etiqueta==levels(shps.tmp@data$Etiqueta)[e]), CatVars$Covariable[var] ]
                                res[e,]<-c(levels(shps.tmp@data$Etiqueta)[e], tapply(tmp.1, tmp.1, length))
                                
                            }##fin for de e
                            
                            res$Especie<-as.character(res$Especie)
                            
                            res[e+1,]<-NA
                            res[e+1,]<- c("Suma",apply(res[,-1], 2, sum2))
                            
                            res[e+2,]<-NA
                            res[e+2,]<- c("Porcentaje",round(apply(res[,-1], 2, sum2)*100/sum(apply(res[,-1], 2, sum2), na.rm=T),2))
                            
                            
                            res[res==0]<-NA
                            res[is.na(res)]<-""
                            write.table(res, paste(wd, "/Resultados/Estadisticas/Frecuencia_", CatVars$Alias[var],".csv", sep=""), sep=",", col.names=T, row.names=F)
                            
                            
                        }
                    }
                    
                    
                    ######## stats vars continuas
                    
                    # print(ConVars$Covariable)
                    
                    
                    if(dim(ConVars)[1]>0){
                        
                        # print("aqui2")
                        # print( head(shps.tmp@data[, ConVars$Covariable] ))
                        # 
                        
                        if(length( levels( shps.tmp@data$Etiqueta ) ) > 1){
                            
                            tmp <- as.data.frame( shps.tmp@data[, ConVars$Covariable])
                            names(tmp)<-ConVars$Alias
                            
                            EstadisticaBasica( tmp,  nfile= paste(wd, "/Resultados/Estadisticas/StatsVarsCont_all.csv", sep="") )
                        }
                        
                        
                        for(f in 1:length( levels( shps.tmp@data$Etiqueta ) )){
                            
                            #  print("aqui3")
                            
                            tmp <- as.data.frame( shps.tmp@data[ which( shps.tmp@data$Etiqueta == levels(shps.tmp@data$Etiqueta)[f] ), ConVars$Covariable ] )
                            names(tmp)<-ConVars$Alias
                            
                            if( dim(tmp)[1]>1 ){
                                
                                #    print("aqui4") 
                                
                                EstadisticaBasica(tmp, nfile=paste(wd, "/Resultados/Estadisticas/StatsVarsCont_", levels(shps.tmp@data$Etiqueta)[f], ".csv", sep=""))
                                
                            }### fin de if
                        }## fin for de f
                        
                    }
                    
                    
                    
                }#### fin de for de ndirs
                
            })##### fin barra de progreso
            remove_modal_spinner()
            
        }else{
            
            show_modal_spinner()
            
            wd<- paste0(input$DirCheckExp, "/", input$checkGroup2Exp )
            #print(wd)
            
            ### crea carpetas de salida
            outDirs<- c(paste0(wd, "/", "Resultados"), 
                        paste0(wd, "/", "Resultados/Estadisticas"), 
                        paste0(wd, "/", "Resultados/Graficas"), 
                        paste0(wd, "/", "Resultados/Modelos") )
            
            sapply(outDirs[!sapply(outDirs, dir.exists)], dir.create)
            
            
            
            f<-list.files(wd, pattern=".shp$", full.names=F, recursive=F)
            #print(f)
            
            
            shps.tmp<-readOGR(paste0(wd,"/", f[1]), verbose=F )
            
            # print("Etiqueta" %in% names(shps.tmp))
            
            if( ("Etiqueta" %in% names(shps.tmp) ) ){
                shps.tmp<-shps.tmp[,c("Etiqueta")]
                
            }else{
                
                shps.tmp@data$Etiqueta<-paste0("Etiqueta no determinada en ", f[1] )
                shps.tmp<-shps.tmp[,c("Etiqueta")]
                
            }
            
            
            if(length(f)>1){
                for(i in 2:length(f)){
                    
                    #  print(f[i])
                    tmpSHP <- readOGR(paste0(wd,"/", f[i]), verbose=F )
                    
                    #print("Etiqueta" %in% names(tmpSHP))
                    
                    
                    if( ("Etiqueta" %in% names(tmpSHP) ) ){
                        
                        tmpSHP<-tmpSHP[,c("Etiqueta")]
                        shps.tmp<-rbind(shps.tmp, tmpSHP)
                        
                    }else{
                        tmpSHP@data$Etiqueta<-paste0("Etiqueta no determinada en ", f[i] )
                        tmpSHP<-tmpSHP[,c("Etiqueta")]
                        shps.tmp<-rbind(shps.tmp, tmpSHP)
                        
                    }   
                    
                }}
            
            crs(shps.tmp)<-""
            crs(CovsExp.geo$map)<-""
            
            ov<- over(shps.tmp, CovsExp.geo$map)
            
            shps.tmp@data<-cbind(shps.tmp@data, ov)
            
            if(input$rmDupExp){shps.tmp<-shps.tmp[!duplicated(shps.tmp@data$control),]}
            
            #print( dim(shps.tmp@data ))
            #print( summary(shps.tmp@data ))
            # print( react_valsExp$df )
            
            CatVars <- react_valsExp$df[ which(react_valsExp$df$Tipo=="factor"), ] 
            ConVars <- react_valsExp$df[ which(react_valsExp$df$Tipo!="factor"), ] 
            
            #print(ConVars)
            #print(CatVars)
            
            ######## stats vars categóricas
            #print(dim(CatVars)[1])
            
            #print(unique(shps.tmp$Etiqueta)) 
            
            if( class(shps.tmp$Etiqueta)!="factor"){shps.tmp$Etiqueta<-as.factor(shps.tmp$Etiqueta)}
            
            # print(levels(shps.tmp$Etiqueta))
            
            if(dim(CatVars)[1]>0){
                for(var in 1:(dim(CatVars)[1]) ){
                    
                    res<-data.frame(Especie=levels(shps.tmp$Etiqueta))
                    res[  levels(   shps.tmp@data[,CatVars$Covariable[var]]   )  ]<-NA
                    
                    #  print(CatVars$Covariable[var])
                    # print( res )
                    # print(names(shps.tmp@data))
                    
                    for(e in 1:length(levels(shps.tmp$Etiqueta))){
                        
                        tmp.1<-shps.tmp@data[which(shps.tmp@data$Etiqueta==levels(shps.tmp@data$Etiqueta)[e]), CatVars$Covariable[var] ]
                        res[e,]<-c(levels(shps.tmp@data$Etiqueta)[e], tapply(tmp.1, tmp.1, length))
                        
                    }##fin for de e
                    
                    res$Especie<-as.character(res$Especie)
                    
                    res[e+1,]<-NA
                    res[e+1,]<- c("Suma",apply(res[,-1], 2, sum2))
                    
                    res[e+2,]<-NA
                    res[e+2,]<- c("Porcentaje",round(apply(res[,-1], 2, sum2)*100/sum(apply(res[,-1], 2, sum2), na.rm=T),2))
                    
                    
                    res[res==0]<-NA
                    res[is.na(res)]<-""
                    write.table(res, paste(wd, "/Resultados/Estadisticas/Frecuencia_", CatVars$Alias[var],".csv", sep=""), sep=",", col.names=T, row.names=F)
                    
                    
                }
            }
            ######## stats vars continuas
            
            
            
            # print(ConVars$Covariable)
            
            
            if(dim(ConVars)[1]>0){
                
                # print("aqui2")
                # print( head(shps.tmp@data[, ConVars$Covariable] ))
                # 
                
                if(length( levels( shps.tmp@data$Etiqueta ) ) > 1){
                    
                    tmp <- as.data.frame( shps.tmp@data[, ConVars$Covariable])
                    names(tmp)<-ConVars$Alias
                    
                    EstadisticaBasica( tmp,  nfile= paste(wd, "/Resultados/Estadisticas/StatsVarsCont_all.csv", sep="") )
                }
                
                
                for(f in 1:length( levels( shps.tmp@data$Etiqueta ) )){
                    
                    #  print("aqui3")
                    
                    tmp <- as.data.frame( shps.tmp@data[ which( shps.tmp@data$Etiqueta == levels(shps.tmp@data$Etiqueta)[f] ), ConVars$Covariable ] )
                    names(tmp)<-ConVars$Alias
                    
                    if( dim(tmp)[1]>1 ){
                        
                        #    print("aqui4") 
                        
                        EstadisticaBasica(tmp, nfile=paste(wd, "/Resultados/Estadisticas/StatsVarsCont_", levels(shps.tmp@data$Etiqueta)[f], ".csv", sep=""))
                        
                    }### fin de if
                }## fin for de f
                
            }
            
            remove_modal_spinner()
            
        }### else de todas las carpetas 
        
        
        
    })   
    
    
    
    
    ######## dibuja las graficas 
    observeEvent(input$runBatchGraf,{
        
        
        if(input$checkGroup2Exp == "Todas las carpetas"){
            
            #print("en proceso")
            
            ipb<-0
            #print("TEST")
            withProgress(message = "Compilando", value = 0, {
                show_modal_spinner()
                
                dirs<-list.dirs(input$DirCheckExp, full.names = F, recursive = F)     
                
                
                for(ndirs in 1:length(dirs)){
                    
                    ipb<-ipb+1
                    incProgress(1/length(dirs), detail = paste("Carpeta: ", dirs[ndirs]))
                    
                    wd<- paste0(input$DirCheckExp, "/", dirs[ndirs] )
                    #print(wd)
                    
                    ### crea carpetas de salida
                    outDirs<- c(paste0(wd, "/", "Resultados"), 
                                paste0(wd, "/", "Resultados/Estadisticas"), 
                                paste0(wd, "/", "Resultados/Graficas"), 
                                paste0(wd, "/", "Resultados/Modelos") )
                    
                    sapply(outDirs[!sapply(outDirs, dir.exists)], dir.create)
                    
                    
                    f<-list.files(wd, pattern=".shp$", full.names=F, recursive=F)
                    #print(f)
                    
                    
                    shps.tmp<-readOGR(paste0(wd,"/", f[1]), verbose=F )
                    
                    # print("Etiqueta" %in% names(shps.tmp))
                    
                    if( ("Etiqueta" %in% names(shps.tmp) ) ){
                        shps.tmp<-shps.tmp[,c("Etiqueta")]
                        
                    }else{
                        
                        shps.tmp@data$Etiqueta<-"Error"
                        shps.tmp<-shps.tmp[,c("Etiqueta")]
                        
                    }
                    
                    
                    if(length(f)>1){
                        for(i in 2:length(f)){
                            
                            #  print(f[i])
                            tmpSHP <- readOGR(paste0(wd,"/", f[i]), verbose=F )
                            
                            #print("Etiqueta" %in% names(tmpSHP))
                            
                            
                            if( ("Etiqueta" %in% names(tmpSHP) ) ){
                                
                                tmpSHP<-tmpSHP[,c("Etiqueta")]
                                shps.tmp<-rbind(shps.tmp, tmpSHP)
                                
                            }
                            
                            # else{
                            #   tmpSHP@data$Etiqueta<-paste0("Etiqueta no determinada en ", f[i] )
                            #   tmpSHP<-tmpSHP[,c("Etiqueta")]
                            #   shps.tmp<-rbind(shps.tmp, tmpSHP)
                            #   
                            # }   
                            
                        }}
                    
                    
                    
                    
                    shps.tmp<- shps.tmp[which(shps.tmp@data$Etiqueta !="Error"), ]
                    
                    crs(shps.tmp)<-""
                    crs(CovsExp.geo$map)<-""
                    
                    ov<- over(shps.tmp, CovsExp.geo$map)
                    
                    shps.tmp@data<-cbind(shps.tmp@data, ov)
                    
                    
                    if( class(shps.tmp$Etiqueta)!="factor"){shps.tmp$Etiqueta<-as.factor(as.character(shps.tmp$Etiqueta))}
                    
                    
                    if(input$rmDupExp){shps.tmp2<-shps.tmp[!duplicated(shps.tmp$control),]}else{shps.tmp2<-shps.tmp}  
                    
                    
                    ConVars <- react_valsExp$df[ which(react_valsExp$df$Tipo!="factor"), ] 
                    
                    
                    for(var in 1:(dim(ConVars)[1])){
                        
                        if(dim(na.omit(shps.tmp2@data))[1] > 1){
                        
                        if(  !file.exists(paste(wd, "/Resultados/", "Graficas", "/", ConVars$Alias[var], ".png", sep=""))  ){
                            
                            #print(ConVars$Covariable[var])
                            #print(  dim(na.omit(shps.tmp2@data))  )
                            #print("aqui")
                            
                            d<-max(density(CovsExp.geo$map@data[,ConVars$Covariable[var]],na.rm=T)$y)
                            #print("aqui2")
                            d1<-max(density(shps.tmp2@data[,ConVars$Covariable[var]],na.rm=T)$y)
                            #print("aqui3")
                            d1<-d1+abs(d1*0.15)
                            
                            d2<-min(min(density(CovsExp.geo$map@data[,ConVars$Covariable[var]],na.rm=T)$x),min(density(shps.tmp2@data[,ConVars$Covariable[var]],na.rm=T )$x))
                            #d2<-d2-abs(d2*0.15)
                            
                            d3<-max(max(density(CovsExp.geo$map@data[,ConVars$Covariable[var]],na.rm=T)$x),max(density(shps.tmp2@data[,ConVars$Covariable[var]],na.rm=T )$x))
                            #d3<-d3+abs(d3*0.15)
                            
                            cols <- randomColor(length(levels(shps.tmp2@data$Etiqueta)), hue = c(" ", "random", "red", "orange", "yellow",
                                                                                                 "green", "blue", "purple", "pink", "monochrome"), luminosity = c(" ",
                                                                                                                                                                  "random", "light", "bright", "dark"))
                            
                            
                            png(paste(wd, "/Resultados/", "Graficas", "/", ConVars$Alias[var], ".png", sep=""), width = 1023, height = 917, pointsize=20)
                            
                            ly<-layout(matrix(c(1, 1, 1,  
                                                1, 1, 1,  
                                                2, 2, 2,  
                                                2, 2, 2)	, nc=3, byrow=T))
                            
                            
                            
                            plot(density(CovsExp.geo$map@data[,ConVars$Covariable[var]],na.rm=T), col="red", lwd=1.5, ylim=c(0,max(d,d1)), xlab=ConVars$Alias[var], ylab="Densidad", 
                                 main=paste("Todas las especies de asociación ", gsub("./", "", dirs[ndirs]), sep=""))
                            lines(density(shps.tmp2@data[,ConVars$Covariable[var]],na.rm=T), lwd=1.5, col="black")
                            legend("top", legend=c("Nacional","Todas las muestras"), col=c("red","black"), lwd=c(2.5,2.5), bty="n", )
                            
                            legend("topright",
                                   paste(
                                       c(names(summary(shps.tmp2@data[,ConVars$Covariable[var]]))),
                                       c(round(summary(shps.tmp2@data[,ConVars$Covariable[var]]),3) ), sep=" "), 
                                   bty="n", text.col=c("grey65","blue","green","brown4","red","grey65","mediumorchid1")
                            )
                            
                            
                            abline(v=round(summary(shps.tmp2@data[,ConVars$Covariable[var]]),3)[1], col="grey65", lty=2, lwd=2)
                            abline(v=round(summary(shps.tmp2@data[,ConVars$Covariable[var]]),3)[2], col="blue", lty=2, lwd=2)
                            abline(v=round(summary(shps.tmp2@data[,ConVars$Covariable[var]]),3)[3], col="green", lty=2, lwd=2)
                            abline(v=round(summary(shps.tmp2@data[,ConVars$Covariable[var]]),3)[4], col="brown4", lty=2, lwd=2)
                            abline(v=round(summary(shps.tmp2@data[,ConVars$Covariable[var]]),3)[5], col="red", lty=2, lwd=2)
                            abline(v=round(summary(shps.tmp2@data[,ConVars$Covariable[var]]),3)[6], col="grey65", lty=2, lwd=2)
                            
                            
                            
                            tmp.1<-shps.tmp2@data[which(shps.tmp2@data$Etiqueta == levels(shps.tmp2@data$Etiqueta)[1]),]
                            plot(density(tmp.1[,ConVars$Covariable[var]],na.rm=T), lwd=1.5, ylim=c(0,max(d,d1)), xlim=c(d2,d3), xlab=ConVars$Alias[var], ylab="Densidad",
                                 main="Densidad por especie", col=cols[1])
                            
                            
                            if(length(levels(shps.tmp2@data$Etiqueta))>1){
                                for(esp in 2:length(levels(shps.tmp2@data$Etiqueta))){
                                    
                                    tmp.1<-shps.tmp2@data[which(shps.tmp2@data$Etiqueta == levels(shps.tmp2@data$Etiqueta)[esp]),]
                                    #str(tmp.1)
                                    
                                    if(dim(tmp.1)[1]>1){
                                        lines(density(tmp.1[,ConVars$Covariable[var]],na.rm=T), lwd=1.5, ylim=c(0,max(d,d1)), xlim=c(d2,d3), xlab=ConVars$Covariable[var],
                                              col=cols[esp])
                                    }### fin if numero de pts en shp
                                    
                                }### fin for esp
                            }### fin if numero de shps
                            
                            
                            legend("topright", legend=paste(levels(shps.tmp2@data$Etiqueta), tapply(shps.tmp@data$Etiqueta,shps.tmp@data$Etiqueta,length ), tapply(shps.tmp2@data$Etiqueta,shps.tmp2@data$Etiqueta,length), sep=" n:"), col=cols, lty=1, bty="n", cex=1, lwd=2)
                            
                            dev.off()
                            
                            
                        }
                        
                    }#### fin for var
                    
                    }
                    
                }### fin for de dirs
                
            })##### fin barra de progreso
            remove_modal_spinner()
            
            
            
        }else{
            
            
            show_modal_spinner()
            
            wd<- paste0(input$DirCheckExp, "/", input$checkGroup2Exp )
            #print(wd)
            
            ### crea carpetas de salida
            outDirs<- c(paste0(wd, "/", "Resultados"), 
                        paste0(wd, "/", "Resultados/Estadisticas"), 
                        paste0(wd, "/", "Resultados/Graficas"), 
                        paste0(wd, "/", "Resultados/Modelos") )
            
            sapply(outDirs[!sapply(outDirs, dir.exists)], dir.create)
            
            
            
            f<-list.files(wd, pattern=".shp$", full.names=F, recursive=F)
            #print(f)
            
            
            shps.tmp<-readOGR(paste0(wd,"/", f[1]), verbose=F )
            
            # print("Etiqueta" %in% names(shps.tmp))
            
            if( ("Etiqueta" %in% names(shps.tmp) ) ){
                shps.tmp<-shps.tmp[,c("Etiqueta")]
                
            }else{
                
                shps.tmp@data$Etiqueta<-"Error"
                shps.tmp<-shps.tmp[,c("Etiqueta")]
                
            }
            
            
            if(length(f)>1){
                for(i in 2:length(f)){
                    
                    #  print(f[i])
                    tmpSHP <- readOGR(paste0(wd,"/", f[i]), verbose=F )
                    
                    #print("Etiqueta" %in% names(tmpSHP))
                    
                    
                    if( ("Etiqueta" %in% names(tmpSHP) ) ){
                        
                        tmpSHP<-tmpSHP[,c("Etiqueta")]
                        shps.tmp<-rbind(shps.tmp, tmpSHP)
                        
                    }
                    
                    # else{
                    #   tmpSHP@data$Etiqueta<-paste0("Etiqueta no determinada en ", f[i] )
                    #   tmpSHP<-tmpSHP[,c("Etiqueta")]
                    #   shps.tmp<-rbind(shps.tmp, tmpSHP)
                    #   
                    # }   
                    
                }}
            
            
            
            
            shps.tmp<- shps.tmp[which(shps.tmp@data$Etiqueta !="Error"), ]
            
            crs(shps.tmp)<-""
            crs(CovsExp.geo$map)<-""
            
            ov<- over(shps.tmp, CovsExp.geo$map)
            
            shps.tmp@data<-cbind(shps.tmp@data, ov)
            
            
            if( class(shps.tmp$Etiqueta)!="factor"){shps.tmp$Etiqueta<-as.factor(as.character(shps.tmp$Etiqueta))}
            
            
            if(input$rmDupExp){shps.tmp2<-shps.tmp[!duplicated(shps.tmp$control),]}else{shps.tmp2<-shps.tmp}  
            
            
            #print( dim(shps.tmp@data ))
            #print( summary(shps.tmp@data ))
            # print( react_valsExp$df )
            
            ConVars <- react_valsExp$df[ which(react_valsExp$df$Tipo!="factor"), ] 
            
            #print(ConVars)
            
            ######## stats vars categóricas
            #print(dim(CatVars)[1])
            
            #print(unique(shps.tmp$Etiqueta)) 
            
            
            
            
            # print(input$checkGroup2Exp)
            
            #CovsExp.geo$map[,ConVars$Covariable[var]]
            
            #shps.tmp@data[,ConVars$Covariable[var]]
            
            
            
            for(var in 1:(dim(ConVars)[1])){
                
                
                if(dim(na.omit(shps.tmp2@data))[1] > 1){
                
                
                if(  !file.exists(paste(wd, "/Resultados/", "Graficas", "/", ConVars$Alias[var], ".png", sep=""))  ){
                    
                    #print(ConVars$Covariable[var])
                    #print("aqui")
                    
                    d<-max(density(CovsExp.geo$map@data[,ConVars$Covariable[var]],na.rm=T)$y)
                    #print("aqui2")
                    d1<-max(density(shps.tmp2@data[,ConVars$Covariable[var]],na.rm=T)$y)
                    #print("aqui3")
                    d1<-d1+abs(d1*0.15)
                    
                    d2<-min(min(density(CovsExp.geo$map@data[,ConVars$Covariable[var]],na.rm=T)$x),min(density(shps.tmp2@data[,ConVars$Covariable[var]],na.rm=T )$x))
                    #d2<-d2-abs(d2*0.15)
                    
                    d3<-max(max(density(CovsExp.geo$map@data[,ConVars$Covariable[var]],na.rm=T)$x),max(density(shps.tmp2@data[,ConVars$Covariable[var]],na.rm=T )$x))
                    #d3<-d3+abs(d3*0.15)
                    
                    cols <- randomColor(length(levels(shps.tmp2@data$Etiqueta)), hue = c(" ", "random", "red", "orange", "yellow",
                                                                                         "green", "blue", "purple", "pink", "monochrome"), luminosity = c(" ",
                                                                                                                                                          "random", "light", "bright", "dark"))
                    
                    
                    png(paste(wd, "/Resultados/", "Graficas", "/", ConVars$Alias[var], ".png", sep=""), width = 1023, height = 917, pointsize=20)
                    
                    ly<-layout(matrix(c(1, 1, 1,  
                                        1, 1, 1,  
                                        2, 2, 2,  
                                        2, 2, 2)	, nc=3, byrow=T))
                    
                    
                    
                    plot(density(CovsExp.geo$map@data[,ConVars$Covariable[var]],na.rm=T), col="red", lwd=1.5, ylim=c(0,max(d,d1)), xlab=ConVars$Alias[var], ylab="Densidad", 
                         main=paste("Todas las especies de asociación ", gsub("./", "", input$checkGroup2Exp), sep=""))
                    lines(density(shps.tmp2@data[,ConVars$Covariable[var]],na.rm=T), lwd=1.5, col="black")
                    legend("top", legend=c("Nacional","Todas las muestras"), col=c("red","black"), lwd=c(2.5,2.5), bty="n", )
                    
                    legend("topright",
                           paste(
                               c(names(summary(shps.tmp2@data[,ConVars$Covariable[var]]))),
                               c(round(summary(shps.tmp2@data[,ConVars$Covariable[var]]),3) ), sep=" "), 
                           bty="n", text.col=c("grey65","blue","green","brown4","red","grey65","mediumorchid1")
                    )
                    
                    
                    abline(v=round(summary(shps.tmp2@data[,ConVars$Covariable[var]]),3)[1], col="grey65", lty=2, lwd=2)
                    abline(v=round(summary(shps.tmp2@data[,ConVars$Covariable[var]]),3)[2], col="blue", lty=2, lwd=2)
                    abline(v=round(summary(shps.tmp2@data[,ConVars$Covariable[var]]),3)[3], col="green", lty=2, lwd=2)
                    abline(v=round(summary(shps.tmp2@data[,ConVars$Covariable[var]]),3)[4], col="brown4", lty=2, lwd=2)
                    abline(v=round(summary(shps.tmp2@data[,ConVars$Covariable[var]]),3)[5], col="red", lty=2, lwd=2)
                    abline(v=round(summary(shps.tmp2@data[,ConVars$Covariable[var]]),3)[6], col="grey65", lty=2, lwd=2)
                    
                    
                    
                    tmp.1<-shps.tmp2@data[which(shps.tmp2@data$Etiqueta == levels(shps.tmp2@data$Etiqueta)[1]),]
                    plot(density(tmp.1[,ConVars$Covariable[var]],na.rm=T), lwd=1.5, ylim=c(0,max(d,d1)), xlim=c(d2,d3), xlab=ConVars$Alias[var], ylab="Densidad",
                         main="Densidad por especie", col=cols[1])
                    
                    
                    if(length(levels(shps.tmp2@data$Etiqueta))>1){
                        for(esp in 2:length(levels(shps.tmp2@data$Etiqueta))){
                            
                            tmp.1<-shps.tmp2@data[which(shps.tmp2@data$Etiqueta == levels(shps.tmp2@data$Etiqueta)[esp]),]
                            #str(tmp.1)
                            
                            if(dim(tmp.1)[1]>1){
                                lines(density(tmp.1[,ConVars$Covariable[var]],na.rm=T), lwd=1.5, ylim=c(0,max(d,d1)), xlim=c(d2,d3), xlab=ConVars$Covariable[var],
                                      col=cols[esp])
                            }### fin if numero de pts en shp
                            
                        }### fin for esp
                    }### fin if numero de shps
                    
                    
                    legend("topright", legend=paste(levels(shps.tmp2@data$Etiqueta), tapply(shps.tmp@data$Etiqueta,shps.tmp@data$Etiqueta,length ), tapply(shps.tmp2@data$Etiqueta,shps.tmp2@data$Etiqueta,length), sep=" n:"), col=cols, lty=1, bty="n", cex=1, lwd=2)
                    
                    dev.off()
                    
                    
                }
                
            }#### fin for var
            
            
            
            }
            
            
            
            remove_modal_spinner()
            
            
        }### fin else carpetas
        
    })   
    
    
    #####    
    
    ###### server archvios hipótesis ####  
    
    
    CovsHip<-reactiveValues(df=data.frame(Covariable=character(), Value=numeric()) )
    
    
    #### crea drop de las carpetas
    output$checkBoxGHip<- renderUI({
        
        if(input$DirCheckHip != ""){
            if(dir.exists(input$DirCheckHip)){
                wd<-input$DirCheckHip   
                dirs<-list.dirs(wd, full.names = F, recursive = F)
                
                if(!is.null( input$covsPrepHip$datapath ) ){
                    selectInput("checkGroup2Hip", label = NULL, choices = c("Todas las carpetas",dirs), selected = "Todas las carpetas") 
                    
                }else{
                    disabled( selectInput("checkGroup2Hip", label = NULL, choices = c("Todas las carpetas",dirs), selected = "Todas las carpetas") )
                }
                
            }else{
                helpText("Error: El direcorio raíz no existe o no se puede tener acceso.")
            }
        }else{
            helpText("Seleccione un directorio raíz")
        }
        
        
    })  
    
    
    ################# tabla 1
    react_valsHip <- reactiveValues(
        df = data.frame(Tipo=character(), Nombre=character(), Sintaxis=character(), Estado=character(), PesoPositivo=numeric(), PesoNegativo=numeric(), stringsAsFactors=F ),
        dt_row = NULL,
        add_or_edit = NULL,
        edit_button = NULL,
        keep_track_id = nrow(df) + 1
    )
    
    ##### crea tabla con el contenido reactivo de react_vals
    output$HipotesisTabla <- renderDataTable(
        {
            isolate(react_valsHip$df)
        },
        escape = F, 
        rownames = FALSE,  caption = tags$caption(style="caption-side: top; text-align: left; margin: 8px 5;","Tabla 1. Hipótesis de salida"),
        options = list(processing = FALSE, ordering=F, paging = T,  language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'), pageLength = 5,
                       headerCallback = JS(
                           "function( thead, data, start, end, display ) {
      $(thead).closest('thead').find('th').css('background-color', '#7A1F1A');
              }"),
      initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'color': 'white'});",
          "}"
      ), autoWidth = F, scrollX=T
      
        ),
      editable = list(target = "cell", disable = list(columns =c(0:1)))
    )
    
    #### proxy de tabla para el manejo interno 
    proxyHip <- dataTableProxy("HipotesisTabla") 
    observe({
        replaceData(proxyHip, react_valsHip$df, resetPaging = FALSE, rownames = FALSE)
    })
    
    
    toListenHips<-reactive({
        list(input$covsPrepHip$datapath, input$DirCheckHip, input$checkGroup2Hip)
        
    })
    
    observeEvent(toListenHips(),{ #input$covsPrepHip
        
        if( !is.null( input$covsPrepHip$datapath ) ){
            show_modal_spinner()
            CovsHip$df <- readRDS(input$covsPrepHip$datapath)
            
            if( any(input$checkGroup2Hip=="Todas las carpetas", is.null(input$checkGroup2Hip) ) ) {
                
                if(dir.exists(input$DirCheckHip)){  
                    react_valsHip$df = data.frame(Tipo=unlist(lapply(CovsHip$df,class))[-c(1,2)], Nombre=names(CovsHip$df)[-c(1,2)], Sintaxis="Q1:Q3", Estado=1, PesoPositivo=0.5, PesoNegativo=0.25, stringsAsFactors=F )
                }else{
                    react_valsHip$df = data.frame(Tipo=character(), Nombre=character(), Sintaxis=character(), Estado=character(), PesoPositivo=numeric(), PesoNegativo=numeric(), stringsAsFactors=F )  
                }
                
                
            }else{
                
                if(dir.exists(input$DirCheckHip)){
                    wd<-paste0(input$DirCheckHip , "/", input$checkGroup2Hip)
                    
                    shps<-list.files(wd, pattern=".shp$", full.names=F, recursive = F)
                    
                    react_valsHip$df = rbind(
                        data.frame(Tipo="spp", Nombre=shps, Sintaxis="", Estado=1, PesoPositivo=NA, PesoNegativo=NA, stringsAsFactors=F ),
                        data.frame(Tipo=unlist(lapply(CovsHip$df,class))[-c(1,2)], Nombre=names(CovsHip$df)[-c(1,2)], Sintaxis="Q1:Q3", Estado=1, PesoPositivo=0.5, PesoNegativo=0.25 ), stringsAsFactors=F)
                }else{
                    react_valsHip$df = data.frame(Tipo=character(), Nombre=character(), Sintaxis=character(), Estado=character(), PesoPositivo=numeric(), PesoNegativo=numeric(), stringsAsFactors=F )  
                }
                
            }
            
            react_valsHip$df$PesoPositivo[which(react_valsHip$df$Tipo=="factor")]<-1
            react_valsHip$df$PesoNegativo[which(react_valsHip$df$Tipo=="factor")]<-0
            
        }
        
        remove_modal_spinner()  
    })
    
    ######## evento para la actualizacion de edicion de celda
    observeEvent(input$HipotesisTabla_cell_edit,{
        ind = input$HipotesisTabla_cell_edit
        #str(info)
        i = ind$row
        j = ind$col+1
        z = ind$value
        # print(paste0(i,"-",j,"-",z))
        react_valsHip$df[i, j] <- isolate(z)
    })
    
    
    ######## boton escribe archivos 
    
    observeEvent(input$creaHip ,{
        wd<-input$DirCheckHip   
        options(show.error.messages = F, warn=-1)
        
        if(input$sufijoHip==""){suf<-Sys.info()[["user"]]}else{suf<-input$sufijoHip}
        
        
        if(input$checkGroup2Hip=="Todas las carpetas"){
            dirs<-list.dirs(wd, full.names = F, recursive = F)
            errorDirs<-""
            
            
            for(ndirs in 1:length(dirs)){
                tmpshps<-list.files( paste0(wd,"/", dirs[ndirs]), pattern=".shp$", full.names=F, recursive = F)
                
                
                res<-rbind(
                    data.frame(Tipo="spp", Nombre=tmpshps, Sintaxis="", Estado=1, PesoPositivo=NA, PesoNegativo=NA, stringsAsFactors=F ),
                    react_valsHip$df
                )
                
                res[is.na(res)]<-""
                
                permiso<- try( write.table(res, paste0(wd,"/",dirs[ndirs],"/","Hipotesis",suf ,".csv"), col.names=T, row.names=F, sep=",") )
                
                if(class(permiso)=="try-error"){
                    
                    errorDirs<- paste0(errorDirs, "<br/>", "Imposible crear ",paste0(".../",dirs[ndirs],"/","Hipotesis",suf ,".csv"))  
                }
                
            }
            
            
            output$errorhelpText <- renderUI({
                HTML(errorDirs)})
            
            showModal( modalDialog(h2("¡Completado!"), #h4("Proceso terminado."),
                                   htmlOutput("errorhelpText"),
                                   footer=modalButton("Cerrar"), size="m"))
            
            
            
        }else{
            
            res<-react_valsHip$df
            res[is.na(res)]<-""
            
            permiso<- try(write.table(res, paste0(wd,"/",input$checkGroup2Hip,"/","Hipotesis",suf ,".csv"), col.names=T, row.names=F, sep=","), silent=T)    
            
            if(class(permiso)=="try-error"){
                
                showModal( modalDialog(h2("¡Error!"), h4("No se puede escribir el archivo."),
                                       helpText(paste0("Archivo: ", ".../",input$checkGroup2Hip,"/","Hipotesis",suf ,".csv" )),
                                       helpText("Posibles soluciones:"),
                                       helpText("El nombre del archivo contiene caracteres no permitidos"),
                                       helpText("No se cuenta con los permisos de escritura necesarios"),
                                       helpText("Cierre las aplicaciones que puedan estar usando el archivo"),footer=modalButton("Cerrar"), size="m" ))
                
            }else{
                write.table(res, paste0(wd,"/",input$checkGroup2Hip,"/","Hipotesis",suf ,".csv"), col.names=T, row.names=F, sep=",")
                showModal( modalDialog(h2("¡Completado!"), h4("El archivo se creó con éxito."),footer=modalButton("Cerrar"), size="m"))
                
            }
            
            
            
        }
        
        
        options(show.error.messages = T, warn=0) 
    })
    
    
    
    ######## desabilitador boton
    observeEvent( list(input$DirCheckHip,input$covsPrepHip, input$sufijoHip ),{
        
        if( all(dir.exists(input$DirCheckHip), !is.null( input$covsPrepHip$datapath ), input$sufijoHip!="" ) ){
            enable("creaHip")
        }else{
            disable("creaHip")
        }
    } )
    
    
    
    
    #####  
    
    ##### server manejo de datos #### 
    
    CovsEdit<-reactiveValues(df=data.frame(Covariable=character(), Value=numeric()) )
    tablaCovsEdit<-reactiveValues(df=data.frame(Covariable=character(), Value=numeric(), stringsAsFactors = F) )
    nameEdited<-reactiveValues(nom="")
    output$EditOpcion0 <- renderUI(textInput("editedCovsOutName", "Guardar como:", placeholder = "---" ))
    rasterToAdd<-reactiveValues(r="")
    
    
    ##### carga datos de covariables
    observeEvent(input$covsLoadEdit,{
        
        if( !is.null( input$covsLoadEdit$datapath ) ){
            show_modal_spinner()
            CovsEdit$df <- readRDS( input$covsLoadEdit$datapath )
            nameEdited$nom<-input$covsLoadEdit$name 
            
            #CovsEdit$df$control<-rownames(CovsEdit$df)
            output$covsInMemory<-renderPrint(
                cat( str(CovsEdit$df[,-c(1:2)] ))
            )
            
            output$EditOpcion0 <- renderUI(textInput("editedCovsOutName", "Guardar como:", placeholder = "---", value = gsub(".rds","_editado",input$covsLoadEdit$name ) ))
            
        }    
        
        remove_modal_spinner() 
    })
    
    
    
    ##### carga datos del nuevo raster 
    observeEvent(input$addRasterVarEdit,{
        
        if( !is.null( input$addRasterVarEdit$datapath ) ){
            show_modal_spinner()
            
            r <- raster( input$addRasterVarEdit$datapath )
            names(r)<-input$addRasterVarEdit$name
            rasterToAdd$r <- r
            
        }    
        
        remove_modal_spinner() 
    })
    
    
    
    
    
    ######### crea dinámicamente la interfaz por herramienta
    
    observeEvent(list(input$toolSelect,input$covsLoadEdit, CovsEdit$df, editControl$n),{
        
        covs <- CovsEdit$df
        if( !is.null( input$covsLoadEdit$datapath ) ){
            
            if(input$toolSelect == "Exportar a shp"){
                
                hide("EditOpcionTable")
                show("EditOpcion1")
                show("EditOpcion2")
                
                output$EditOpcion1<- renderUI( textInput("outShpEdit", label = "Asigne un nombre al archivo", placeholder = "---", value=sub(".rds","",nameEdited$nom) ) )
                output$EditOpcion2<- renderUI( textInput("outDirEdit", label = "Dirección de salida", placeholder = "---") )
                
            }else if(input$toolSelect == "Exportar a Raster"){
                
                hide("EditOpcionTable")
                show("EditOpcion1")
                show("EditOpcion2")
                
                output$EditOpcion1<- renderUI( selectInput("covsNamesEdit", "Exportar variable:", selected = "---", choices = c("---",names(covs)[-c(1:2)]) ) )
                output$EditOpcion2<- renderUI( textInput("outDirEdit", label = "Dirección de salida", placeholder = "---") )
                
                
                
            }else if(input$toolSelect == "Editar clases"){
                
                tmp<-data.frame(Clase=character(), ClaseNueva=character(), Conteo=numeric(),  stringsAsFactors = F)
                
                tablaCovsEdit$df<-tmp
                
                show("EditOpcion1")
                show("EditOpcion2")
                show("EditOpcionTable")
                
                fact<-as.data.frame(covs[, lapply(covs,class) =="factor"])
                
                names(fact)<-names(covs)[lapply(covs,class) =="factor"]
                
                
                output$EditOpcion1<- renderUI( selectInput("covsNamesEdit", "Modificar clases de:", selected = "---", choices = c("---",names(fact)) ) )
                #print(input$covsNamesEdit)
                
                
                output$EditOpcion2<-  renderUI(HTML(paste0(
                    "<FONT color='red'>Advertencia</FONT>: Los campos sin asignar serán agrupados y etiquetados con la clase 'Sin asignar'.")))
                
                
                
            }else if(input$toolSelect == "Editar nombres de atributos"){
                
                show("EditOpcionTable")
                hide("EditOpcion1")
                
                tmp<-data.frame(Nombre=names(covs)[-c(1:2)], NombreNuevo=names(covs)[-c(1:2)],  stringsAsFactors = F)
                
                tablaCovsEdit$df<-tmp
                
                
                output$EditOpcionTable <- renderDataTable(
                    {
                        isolate(tablaCovsEdit$df)
                    },
                    escape = F, 
                    rownames = FALSE,  #caption = tags$caption(style="caption-side: top; text-align: center; margin: 8px 5;","Tabla 1. Datos de entrada para la armonización"),
                    options = list(processing = F, ordering=T, paging = T,  language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'), 
                                   headerCallback = JS(
                                     "function( thead, data, start, end, display ) {
      $(thead).closest('thead').find('th').css('background-color', '#7A1F1A');
              }"),
                                   initComplete = JS(
                                     "function(settings, json) {",
                                     "$(this.api().table().header()).css({'color': 'white'});",
                                     "}"
                                   ), autoWidth = F, scrollX=F
                                   
                    ),
                    editable = list(target = "cell", disable = list(columns =c(0,2)))
                )
                
                
                output$EditOpcion2<-  renderUI(HTML(paste0(
                    "<FONT color='red'>Advertencia</FONT>: Los nombres dejados en blanco tomarán el nombre original del atributo.")))
                
                
            }else if(input$toolSelect == "Agregar covariable"){
                
                show("EditOpcion1")
                show("EditOpcion2")  
                hide("EditOpcion3")  
                hide("EditOpcionTable")  
                
                
                output$EditOpcion1<- renderUI( 
                    fluidRow(column(12,
                                    div(style="display: inline-block;vertical-align:center;width: 60%", fileInput("addRasterVarEdit","Nueva variable", multiple = F,accept = ".tif",buttonLabel = "Buscar", placeholder = "Seleccione un Geotiff") ),
                                    div(style="display: inline-block;vertical-align:top;width: 15%", textInput("addRasterType", "Tipo de dato:", placeholder = "---", value = 1) ),
                                    div(style="display: inline-block;vertical-align:top;width: 15%", textInput("addRasterNAval", "Valor para los NA:", placeholder = "", value = "") )
                    ))
                    
                )
                
                output$EditOpcion2<-  renderUI(HTML(paste0(
                    "<FONT color='red'>Advertencia</FONT>: Para la opción 'Tipo de dato' solo se puede asignar; 1 para variables continuas y 2 para variables categóricas.
          Para 'Valor para los NA', dejar en blanco si no se cambiarán los valores NA (elimina esos píxeles) o asigne un valor compatible con el tipo de la variable")))
                
                
                
            }else if(input$toolSelect == "Eliminar covariable"){
                
                hide("EditOpcionTable")
                show("EditOpcion1")
                show("EditOpcion2")
                
                output$EditOpcion1<- renderUI( selectInput("covsNamesEdit", "Eliminar variable:", selected = "---", choices = c("---",names(covs)[-c(1:2)]) ) )
                output$EditOpcion2<- renderUI(HTML(paste0(
                    "<FONT color='red'>Advertencia</FONT>: Una vez eliminada una covariable será imposible recuperarla en esta sessión.</b>
                Si eliminó una variable por error, cargue nuevamente el archivo de covariables original.")))
                
            }else{
                
                hide("EditOpcion1")
                hide("EditOpcion2")
                hide("EditOpcion3")
                
                hide("EditOpcionTable")
                
                
            }
            
        }else{
            
            hide("EditOpcionTable")
            show("EditOpcion1")
            output$EditOpcion1<- renderUI( h4("Seleccione un archivo de covariables") )
            
        }#### else de verificacion que covs existe
        
    })
    
    
    
    ######## evento para cambio de seleccion de variable
    
    observeEvent(input$covsNamesEdit,{
        covs <- CovsEdit$df
        
        fact<-covs[, lapply(covs,class)=="factor"]
        
        if(input$toolSelect == "Editar clases"){
            
            if( all(input$covsNamesEdit != "---", !is.null(input$covsNamesEdit)) ){
                
                show("EditOpcionTable")
                hide("EditOpcion3")
                
                tmp<-tapply(covs[,input$covsNamesEdit], covs[,input$covsNamesEdit], length)
                tmp<-data.frame(Clase=names(tmp), ClaseNueva=names(tmp), Conteo=as.vector(tmp), stringsAsFactors = F)
                
                tablaCovsEdit$df<-tmp
                
                #print(tablaCovsEdit$df)
                output$EditOpcionTable <- renderDataTable(
                    {
                        isolate(tablaCovsEdit$df)
                    },
                    escape = F, 
                    rownames = FALSE,  #caption = tags$caption(style="caption-side: top; text-align: center; margin: 8px 5;","Tabla 1. Datos de entrada para la armonización"),
                    options = list(processing = T, ordering=T, paging = T,  language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'), 
                                   headerCallback = JS(
                                     "function( thead, data, start, end, display ) {
      $(thead).closest('thead').find('th').css('background-color', '#7A1F1A');
              }"),
                                   initComplete = JS(
                                     "function(settings, json) {",
                                     "$(this.api().table().header()).css({'color': 'white'});",
                                     "}"
                                   ), autoWidth = F, scrollX=F
                                   
                    ),
                    editable = list(target = "cell", disable = list(columns =c(0,2)))
                )
                
                
            }else{
                show("EditOpcion3")
                output$EditOpcion3<-  renderUI( h4("Seleccione una covariable") )
                hide("EditOpcionTable")
            }
            
        }
        
    })
    
    
    
    ########### boton para ejecutar la herramienta
    editControl<-reactiveValues(n=0)
    
    observeEvent(input$btn_aplicarCambiosEdit,{
        show_modal_spinner()  
        
        if(input$toolSelect == "Exportar a shp"){
            
            if( all(c("x","y") %in% names(CovsEdit$df)) ){
                
                tmp<-CovsEdit$df
                coordinates(tmp)<-~x+y
                shapefile(tmp, paste0(input$outDirEdit,"/",input$outShpEdit, ".shp"))
                
                remove_modal_spinner() 
                
            }else{
                
                remove_modal_spinner() 
                output$errorCovsXY<- renderUI({
                    HTML(paste0(
                        "El archivo de covariables no contiene campos de coordenadas [x, y].<br/><br/>
                        Seleccione un archivo válido, se recomienda usar solo archivos generados <br/>
                        con la herramienta de Armonización cartográfica"
                    ))})
                
                showModal( modalDialog(h2("¡Error!"), 
                                       htmlOutput("errorCovsXY"),
                                       footer=modalButton("Cerrar"), size="m"))
                
            }
            
        }
        else if(input$toolSelect == "Exportar a Raster"){
            
            if( all(c("x","y") %in% names(CovsEdit$df)) ){
                
                if(class(CovsEdit$df[,input$covsNamesEdit]) != "factor"){
                    
                    tmp<-CovsEdit$df
                    coordinates(tmp)<-~x+y
                    gridded(tmp)<-T
                    r<-raster( tmp[,input$covsNamesEdit] )
                    print(r)
                    writeRaster(r, paste0(input$outDirEdit,  "/", input$covsNamesEdit, ".tif"), overwrite=T )
                    
                    
                }else{
                    
                    tmp<-CovsEdit$df
                    
                    tmp[input$covsNamesEdit] <- as.numeric( unlist(tmp[input$covsNamesEdit]) )
                    coordinates(tmp)<-~x+y
                    gridded(tmp)<-T
                    
                    r<-raster( tmp[input$covsNamesEdit] )
                    print(r)
                    
                    clases<-data.frame(
                        Clase=names(tapply(CovsEdit$df[,input$covsNamesEdit], CovsEdit$df[,input$covsNamesEdit], unique)),
                        Valor=as.vector(tapply(CovsEdit$df[,input$covsNamesEdit], CovsEdit$df[,input$covsNamesEdit], unique))
                    )
                    
                    writeRaster(r, paste0(input$outDirEdit,  "/", input$covsNamesEdit, ".tif"), overwrite=T)
                    write.table(clases, paste0(input$outDirEdit,  "/", input$covsNamesEdit, "_clases.csv"), col.names=T, row.names=F, sep="," )
                    
                }
                
                
                remove_modal_spinner() 
                
            }else{
                
                remove_modal_spinner() 
                output$errorCovsXY<- renderUI({
                    HTML(paste0(
                        "El archivo de covariables no contiene campos de coordenadas [x, y].<br/><br/>
                        Seleccione un archivo válido, se recomienda usar solo archivos generados <br/>
                        con la herramienta de Armonización cartográfica"
                    ))})
                
                showModal( modalDialog(h2("¡Error!"), 
                                       htmlOutput("errorCovsXY"),
                                       footer=modalButton("Cerrar"), size="m"))
                
            }
            
            
        }
        else if(input$toolSelect == "Editar clases"){
            
            new.clase<- tablaCovsEdit$df$ClaseNueva
            new.clase[new.clase==""]<-"Sin asignar"
            
            CovsEdit$df[input$covsNamesEdit]<-as.character(CovsEdit$df[,input$covsNamesEdit])
            CovsEdit$df[input$covsNamesEdit] <- factor(CovsEdit$df[,input$covsNamesEdit], levels = tablaCovsEdit$df$Clase, labels= new.clase, ordered=F)
            
            remove_modal_spinner()
            
        }
        else if(input$toolSelect == "Editar nombres de atributos"){
            
            new.nombre<-tablaCovsEdit$df$NombreNuevo
            new.nombre[new.nombre==""] <- tablaCovsEdit$df$Nombre[new.nombre==""]
            
            names(CovsEdit$df)<-c("x","y", new.nombre )
            
            output$covsInMemory<-renderPrint(
                cat( str(CovsEdit$df[,-c(1:2)] ))
            )
            
            editControl$n<-editControl$n+1  
            remove_modal_spinner()
            
        }
        else if(input$toolSelect == "Agregar covariable"){
            
            dat.geo<-CovsEdit$df
            coordinates(dat.geo)<-~x+y
            gridded(dat.geo)<-T
            
            ov<-extract(rasterToAdd$r, dat.geo)
            
            if( input$addRasterNAval != ""){
                ov[is.na(ov)]<-input$addRasterNAval
            }
            
            if(input$addRasterType == 1){
                ov<-as.numeric(ov)
            }else{
                ov<-as.factor(ov)
                
            }
            
            CovsEdit$df[ sub(".tif","", names(rasterToAdd$r)) ]<-ov
            
            CovsEdit$df<-na.omit(CovsEdit$df)
            
            output$covsInMemory<-renderPrint(
                cat( str(CovsEdit$df[,-c(1:2)] ))
            )
            
            
            remove_modal_spinner()
        }
        else if(input$toolSelect == "Eliminar covariable"){
            
            CovsEdit$df[,input$covsNamesEdit]<-NULL
            output$covsInMemory<-renderPrint(
                cat( str(CovsEdit$df[,-c(1:2)] ))
            )
            
            remove_modal_spinner()
            
        }
        else{
            showModal( modalDialog(h2("¡Error!"), 
                                   htmlOutput("Herramienta no implementada"),
                                   footer=modalButton("Cerrar"), size="m"))
            
        }
        
        
    })
    
    
    
    
    #### proxy de tabla para el manejo interno 
    proxyTableEdit <- dataTableProxy("EditOpcionTable") 
    observe({
        replaceData(proxyTableEdit, tablaCovsEdit$df, resetPaging = FALSE, rownames = FALSE)
    })
    
    
    
    ######## evento para la actualizacion de edicion de celda
    observeEvent(input$EditOpcionTable_cell_edit,{
        ind = input$EditOpcionTable_cell_edit
        #str(info)
        i = ind$row
        j = ind$col+1
        z = ind$value
        #print(paste0(i,"-",j,"-",z))
        tablaCovsEdit$df[i, j] <- isolate(z)
        
        #print(tablaCovsEdit$df)
    })
    
    
    ########### desabilitador boton aplicar cambios
    
    observeEvent(list(input$toolSelect, input$outShpEdit, input$covsNamesEdit, input$outDirEdit, input$addRasterVarEdit),{
        
        if(input$toolSelect == "Exportar a shp" ){
            if( all(input$outShpEdit!="", !is.null(input$outDirEdit), input$outDirEdit!="" ) ){
                if(dir.exists(input$outDirEdit)){
                    enable("btn_aplicarCambiosEdit")
                }else{
                    disable("btn_aplicarCambiosEdit")
                }
            }else{
                disable("btn_aplicarCambiosEdit")
            }
        }
        else if(input$toolSelect == "Exportar a Raster"){
            
            if( all(input$covsNamesEdit != "---", !is.null(input$outDirEdit), input$outDirEdit!="" ) ){
                if(dir.exists(input$outDirEdit)){
                    enable("btn_aplicarCambiosEdit")
                }else{
                    disable("btn_aplicarCambiosEdit")
                }
            }else{
                disable("btn_aplicarCambiosEdit")
            }
        }
        else if(input$toolSelect == "Editar clases"){
            if( !is.null(input$covsNamesEdit) ){
                if(input$covsNamesEdit != "---" ){
                    enable("btn_aplicarCambiosEdit")
                }else{
                    disable("btn_aplicarCambiosEdit")
                }
            }else{
                disable("btn_aplicarCambiosEdit")
            }
        }
        else if(input$toolSelect == "Editar nombres de atributos"){
            enable("btn_aplicarCambiosEdit")
        }
        else if(input$toolSelect == "Agregar covariable"){
            if(!is.null( input$addRasterVarEdit$datapath )){
                enable("btn_aplicarCambiosEdit")
            }else{
                disable("btn_aplicarCambiosEdit")
            }
        }
        else if(input$toolSelect == "Eliminar covariable"){
            if( !is.null(input$covsNamesEdit) ){
                if(input$covsNamesEdit != "---"){
                    enable("btn_aplicarCambiosEdit")
                }else{
                    disable("btn_aplicarCambiosEdit")
                }
            }else{
                disable("btn_aplicarCambiosEdit")
            }
        }
        else{
            disable("btn_aplicarCambiosEdit")
        }
        
    })
    
    
    ####### manejo/validacion para tipo de nueva capa 
    observeEvent(input$addRasterType,{
        if( !any(input$addRasterType %in% c("1", "2") ) ){
            reset("addRasterType")
        }
    } )
    
    
    ####### manejo/validacion para el NA de nueva capa 
    
    observeEvent( list(input$addRasterNAval, input$addRasterType) ,{
        if(!is.null(input$addRasterNAval)){
            if(input$addRasterType == 1){
                if( is.na( as.numeric(input$addRasterNAval) ) ){
                    reset("addRasterNAval")
                }
            }
        }
    })
    
    
    ####### evento para guardar en archivo el nuevo rds de covariables
    
    observeEvent(input$btn_GuardarEdit,{
        show_modal_spinner()
        saveRDS(CovsEdit$df, paste0(input$editedCovsOutDir,"/",input$editedCovsOutName, ".rds"))
        remove_modal_spinner()
    })
    
    
    ########### desabilitador boton guardar
    
    observeEvent(list(input$covsLoadEdit, input$editedCovsOutDir, input$editedCovsOutName),{
        if( all( !is.null( input$covsLoadEdit$datapath ), dir.exists(input$editedCovsOutDir), input$editedCovsOutName != "" ) ){
            enable("btn_GuardarEdit")
        }else{
            disable("btn_GuardarEdit")
        }
    })
    
    
    
    
    
    
    
    #####  
    
    
    ###### server ensamble #####  
    
    ensamblePols<-reactiveValues(polys="")
    ensambleName<-reactiveValues(name="")
    wd<-reactiveValues(out="")
    shpMapExportMaps<-reactiveValues(df="")
    
    
    #### direccion de salida
    observeEvent(input$DirCheckEnsamble, {
        
        output$outDirEnsamble<- renderUI({
            if( any(is.null(input$DirCheckEnsamble), input$DirCheckEnsamble == "") ){
                helpText("Seleccione un directorio de trabajo")
            }else if(dir.exists(input$DirCheckEnsamble) ){
                dtemp<-strsplit(input$DirCheckEnsamble ,"/")[[1]]
                #  helpText( pasteFit( dtemp[-length(dtemp)] , sep="/" ))
                
                helpText(  gsub(dtemp[length(dtemp)],"",input$DirCheckEnsamble) )
                
            }else if(!dir.exists(input$DirCheckEnsamble)){
                helpText( "Error, el directorio no existe o no se puede tener acceso.")
            }else{
                helpText("Error en directorio de salida")
            }
        })
    })
    
    
    ###### modelos disponibles
    observeEvent( list(input$DirCheckEnsamble, input$filtroEnsamble) ,{
        
        #print(input$filtroEnsamble)
        #temp<-strsplit(input$DirCheckEnsamble ,"/")[[1]]
        
        output$ListaModelos<-renderUI({
            
            if(dir.exists(input$DirCheckEnsamble) ){
                mods <- list.files(input$DirCheckEnsamble, pattern=".tif$", recursive = T, full.names = F)
                mods<-mods[!grepl("_Clases", mods)]
                
                if( all(input$filtroEnsamble!="---", input$filtroEnsamble!="") ){
                    mods<-mods[grepl(input$filtroEnsamble, mods)] 
                }
                
                checkboxGroupInput("EnsambleModelos", "Modelos de entrada", choices = mods, selected = mods, inline = F ) 
                
            }else{
                helpText("Asigne el directorio de trabajo")
            }
            
        })
        
    })
    
    
    ###### N modelos selecionados 
    
    observeEvent(list(input$EnsambleModelos, input$DirCheckEnsamble),{
        
        if(dir.exists(input$DirCheckEnsamble) ){
            output$NmodelosSelected <- renderUI(helpText(paste0(length(input$EnsambleModelos)," modelos seleccionados") ) )
        }else{
            output$NmodelosSelected <- renderUI(helpText( "Sin resultados" ) ) 
        }
        
    })
    
    ######## desabilitador ensamble
    
    observeEvent( list(input$DirCheckEnsamble, input$EnsambleModelos, input$outDirEnsamble, input$outNameEnsamble) ,{
        #print(input$DirCheckEnsamble)
        
        if( all( !is.null(input$DirCheckEnsamble), dir.exists(input$DirCheckEnsamble), (length(input$EnsambleModelos) >= 2), input$outDirEnsamble !="", input$outNameEnsamble != "" ) ){
            enable("btnIniciarEnsamble")
        }else{
            disable("btnIniciarEnsamble")
        }
        
    })
    
    
    ####### inicia ensamble
    
    observeEvent(input$btnIniciarEnsamble,{
        
        #input$DirCheckEnsamble
        dtemp<-strsplit(input$DirCheckEnsamble ,"/")[[1]]
        
        
        wd.out<-paste0(gsub(dtemp[length(dtemp)], "", input$DirCheckEnsamble), input$outDirEnsamble)
        wd<-input$DirCheckEnsamble
        
        print("wd:")
        print(wd)
        print("wd.out:")
        print(wd.out)
        
        try( dir.create(wd.out), silent = F )
        
        if( !dir.exists(wd.out)){
            
            showModal( modalDialog(h2("¡Error!"), h4("No se puede crear la carpeta de salida"),
                                   helpText("El nombre de la carpeta contine caracteres inválidos"),
                                   helpText("o no se tienen los permisos necesarios"),
                                   footer=modalButton("Cerrar"), size="m"))
        }else{
            
            print("Empieza")
            
            withProgress(message = "Ensamblando:", value = 0, {
                show_modal_spinner()
                
                incProgress(1/5, detail =  "Inicializando")
                
                
                log.tmp <- file.path( paste0(wd.out,"/", input$outNameEnsamble, ".log" ) )
                lf <- log_open(log.tmp, show_notes=F, logdir = F)
                
                
                log_print("---- Modelos ensamblados -----", console=F)
                log_print(input$EnsambleModelos, console=F)
                
                
                fnames<-gsub("/Resultados/Modelos/Modelo_Hipotesis_","",input$EnsambleModelos)
                fnames<-gsub(".tif","", fnames)
                
                log_print("---- Resumen de entrada de datos -----", console = F)
                
                s<-stack(paste0(wd, "/",input$EnsambleModelos))
                names(s)<-paste0("M", fnames)
                
                log_print(s, console = F)
                
                ##### raster a data frame
                datos<-as.data.frame(s ,xy=T)
                datos<-na.omit(datos)
                
                log_print(ls.str( pattern="datos"  ), console =F)
                log_print(head(datos), console = F)
                
                incProgress(1/5, detail =  "Ordenando predicciones")
                
                ##### ordena ensamble y regresa valor 
                dat.res_OG<-apply(datos[,c(-1,-2)], 1, ensambleNum_OG, id=names(datos[,c(-1,-2)]) )
                
                aa<-dat.res_OG
                aa<-sapply(aa, FUN=strsplit, " ")
                aa<-sapply(aa, FUN=unlist)
                aa<-unlist(aa)
                aa1<-as.data.frame(matrix(data = aa, nrow =dim(datos)[1], ncol =  dim(datos)[2]-2, byrow = T,dimnames = NULL))
                
                ##### ordena ensamble y regresa el nombre 
                dat.res_OG_id<-apply(datos[,c(-1,-2)], 1, ensambleID_OG, id=names(datos[,c(-1,-2)]) )
                
                aa.id<-dat.res_OG_id
                aa.id<-sapply(aa.id, FUN=strsplit, " ")
                aa.id<-sapply(aa.id, FUN=unlist)
                aa.id<-unlist(aa.id)
                aa1.id<-as.data.frame(matrix(data = aa.id, nrow =dim(datos)[1], ncol =  dim(datos)[2]-2, byrow = T,dimnames = NULL))
                
                ##### cambia valores a formato correcto
                aa1<-as.data.frame( apply(aa1, 2, as.character ), stringsAsFactors =F)
                aa1<-as.data.frame( apply(aa1, 2, as.numeric ), stringsAsFactors =F)
                
                aa1.id<-as.data.frame( apply(aa1.id, 2, as.character ), stringsAsFactors =F)
                
                incProgress(1/5, detail =  "Preparando archivo de salida")
                
                
                
                
                ensamble<-datos[,c("x","y")]
                ensamble$ID<-1:dim(ensamble)[1]
                
                coordinates(ensamble)<-~x+y
                
                ensamble@data<-cbind(ensamble@data, aa1, aa1.id)
                
                
                #str(ensamble@data)
                names(ensamble@data)<-c("ID",
                                        paste0("VDom", 1:(dim(datos)[2]-2) ),
                                        paste0("NDom", 1:(dim(datos)[2]-2) )
                )
                
                
                #map<-ensamble[,"ID"]
                
                
                print( str(ensamble@data) )
                
                
                ####  si la salida es poyls 
                #incProgress(1/5, detail =  "Creando polgígonos")
                #r<-raster(map)
                #rPols<-rasterToPolygons(r)
                #rPols@data<-join(rPols@data, ensamble@data, by="ID")
                
                incProgress(1/5, detail =  "Creando capa vectorial")
                
                rPols<-ensamble
                
                
                incProgress(1/5, detail =  "Guardando archivos")
                
                shapefile(rPols, paste0(wd.out,"/", input$outNameEnsamble, ".shp" ), overwrite=TRUE )
                
                
                save("rPols", "wd.out", file=paste0(wd.out,"/", input$outNameEnsamble, ".RData" ) )
                
                
                log_print("---- Archivos creados -----", console = F)
                
                log_print(paste0(wd.out,"/", input$outNameEnsamble, ".shp" ), console = F)
                log_print(paste0(wd.out,"/", input$outNameEnsamble, ".RData" ), console = F)
                
                log_close()
                
            })### fin barra de progreso
            
        }### fin else de crea carpeta salida
        
        remove_modal_spinner()
        
    } )
    
    
    #################  exportar mapas
    
    output$dimensionesEnsamble<-renderUI(helpText("Seleccione un archivo de tipo ensamble"))
    
    ### carga datos del ensamble 
    observeEvent(input$ResEnsamble,{
        
        if( !is.null( input$ResEnsamble$datapath ) ){
            
            # print(input$ResEnsamble)
            
            load(input$ResEnsamble$datapath)
            
            ensambleName$name <- gsub(".RData", "",input$ResEnsamble$name)
            ensamblePols$polys<-rPols
            
            #print(ls())
            
            
            if( dir.exists(wd.out) ){
                output$outDirMapas<- renderUI(helpText( paste0(".../", pasteFit(strsplit(wd.out,"/")[[1]][ (length(strsplit(wd.out,"/")[[1]])-4):length(strsplit(wd.out,"/")[[1]]) ], sep="/")) ))
                output$outDirMapas.new<- renderUI(helpText(""))
                wd$out<-wd.out
            }else{
                output$outDirMapas<- renderUI(helpText("No se encontro la ruta de salida original, asigne una nueva ruta de salida"))
                output$outDirMapas.new<- renderUI(textInput("wd.out.new", "Salvar en:", placeholder = "Asigne una nueva ruta de salida", width = "100%"))
            }
            
            
            output$dimensionesEnsamble<-renderUI({
                capasName<- names(rPols@data)[-1]
                capasName<- capasName[grepl("N",capasName)]
                
                selectInput("exportEnsamble","Exportar:", choices = c("---", capasName), selected = "---")
            })
            
            
        }
        
        
    })
    
    #### evento ruta salida nueva
    
    observeEvent(input$wd.out.new,{
        wd$out<-input$wd.out.new
    })
    
    
    
    ####### Inicia exportacion de mapas
    observeEvent(input$btnExportarMapas,{
        
        show_modal_spinner()
        
        #print(ensambleName$name)
        
        capa<-input$exportEnsamble
        ptsPols<- cbind(as.data.frame(coordinates(ensamblePols$polys)),  as.data.frame(ensamblePols$polys) )
        
        names(ptsPols)[1:2]<-c("x","y")
        coordinates(ptsPols)<-~x+y
        gridded(ptsPols)<-T
        
        
        if( dir.exists(wd$out) ){
            
            ### exporta png
            if(input$exportPNG){
                
                if( !dir.exists(paste0(wd$out,"/","MapasPNG")) ) {dir.create(paste0(wd$out,"/","MapasPNG"))}
                
                ptsPols.capa <- ptsPols
                ptsPols.capa@data <- ptsPols@data[, c(sub("N","V", capa) ,  capa)]
                u<-unique(ptsPols.capa@data[,capa])
                
                for(i in 1:length(u)){
                    
                    map<-ptsPols.capa[ which(ptsPols.capa@data[,capa]==u[i] ),]
                    
                    ### continuos
                    map.tmp <- map[,1]
                    r<-raster(map.tmp)
                    png( paste0(wd$out,"/","MapasPNG", "/EnsambleContinuo_", sub("N","", capa), "_", u[i],".png"), width = 480*3, height = 480*2, pointsize = 18)
                    plot(r, main=paste0("Ensamble para ", u[i] ,sub("N"," en ", capa), "[Variable continua]"), ext=extent(map) )
                    if( class(shpMapExportMaps$df) %in% c("SpatialLinesDataFrame", "SpatialPointsDataFrame") ){ plot(shpMapExportMaps$df, add=T, col="black", pch=19)}
                    dev.off()
                    
                    ### clases
                    map.tmp <- map[,2]
                    names(map.tmp)<-"z"
                    map.tmp$z<-1
                    r<-raster(map.tmp)
                    png( paste0(wd$out,"/","MapasPNG", "/EnsambleClase_", sub("N","", capa), "_", u[i], ".png" ), width = 480*3, height = 480*2, pointsize = 18)
                    plot(r, col="red", legend=F, main=paste0("Ensamble para ", u[i] ,sub("N"," en ", capa), "[Extensión]"))
                    if( class(shpMapExportMaps$df) %in% c("SpatialLinesDataFrame", "SpatialPointsDataFrame") ){ plot(shpMapExportMaps$df, add=T, col="black", pch=19)}
                    dev.off()
                    
                }
            }
            
            
            ### exporta geotiff
            if(input$exportRaster){
                
                if( !dir.exists(paste0(wd$out,"/","MapasGeoTiff")) ) {dir.create(paste0(wd$out,"/","MapasGeotiff"))}
                
                ptsPols.capa <- ptsPols
                ptsPols.capa@data <- ptsPols@data[, c(sub("N","V", capa) ,  capa)]
                u<-unique(ptsPols.capa@data[,capa])
                
                for(i in 1:length(u)){
                    
                    map<-ptsPols.capa[ which(ptsPols.capa@data[,capa]==u[i] ),]
                    
                    ### continuos
                    map.tmp <- map[,1]
                    r<-raster(map.tmp)
                    writeRaster(r, paste0(wd$out,"/","MapasGeotiff", "/EnsambleContinuo_", sub("N","", capa), "_", u[i],".tif"), overwrite=TRUE)
                    
                    #### clases
                    map.tmp <- map[,2]
                    names(map.tmp)<-"z"
                    map.tmp$z<-1
                    r<-raster(map.tmp)
                    writeRaster(r, paste0(wd$out,"/","MapasGeotiff", "/EnsambleClase_", sub("N","", capa), "_", u[i], ".tif" ), overwrite=TRUE )
                    
                }
            }  
          
          
          ### exporta tablas
          if(input$exportTablasEnsamble){
            
            if( !dir.exists(paste0(wd$out,"/","ResumenTablas")) ) {dir.create(paste0(wd$out,"/","ResumenTablas"))}
            
            
            
            res<-data.frame(Grupo=levels(  as.factor(ptsPols@data$NDom1))  )
            res$Area<-NA
            res[  , levels(  as.factor(ptsPols@data$NDom1)  ) ]<-NA
            
            
            tmpPols<- ptsPols@data[, grepl("N", names(ptsPols@data))]
            
            
            tmpPols<-as.data.frame(apply(tmpPols, 2,as.factor), stringsAsFactors = T)
            
         
            
            ##### tabla dom1 vs dom2
            for(i in 1:length(   levels(tmpPols$NDom1  ) ) ){
              
              tmp<-tmpPols[which( tmpPols$NDom1 == levels(  tmpPols$NDom1  )[i] ), ]
              
              res[i,3:dim(res)[2]]<- round((tapply(tmp$NDom2, tmp$NDom2 , length )*100)/dim(tmp)[1],2)
              res[i,2]<-dim(tmp)[1]
              
            }
            
            res[is.na(res)]<-""
            write.table(res, paste0(wd$out,"/","ResumenTablas/ResumenTabular_DominanteVSsubdominante.csv"), sep=",", col.names=T, row.names = F )
            
            
          
          ###### todas las tablas
          
          for(j in 1:length(  levels(tmpPols$NDom1)  ) ){
            
            
            
            tmp<-tmpPols[which(tmpPols$NDom1 == levels(tmpPols$NDom1)[j]),]
            #dim(tmp)
            #str(tmp)
            
            res<-data.frame(Grupo=levels(tmp$NDom1) )
            res
            
            for(i in 1:length(  levels(tmp$NDom1)  )){
              res[,(i+1)]<-round( tapply(tmp$NDom1,tmp[,(i)], length )*100/dim(tmp)[1], 2)
            }
            
            names(res)[2: (length(  levels(tmp$NDom1)  )+1) ]<-paste("Dom",1:length(  levels(tmp$NDom1)  ),sep="")
            res<-res[order(res$Dom2, decreasing=T),]
            res[is.na(res)]<-""
            res$Dom1[res$Dom1=="100"]<-dim(tmp)[1]
            
            write.table(res, paste0(wd$out,"/","ResumenTablas/ResumenTabular_Dominante_",levels(tmpPols$NDom1)[j],".csv"), sep=",", col.names=T, row.names = F )
            
          }}
          
          
          
 
          
          
          
          
          
            
        }else{
            
            showModal( modalDialog(h2("¡Error!"), 
                                   helpText("No se puede tener acceso a la carpeta de salida"),
                                   helpText("asigne una dirección válida"),
                                   footer=modalButton("Cerrar"), size="m"))  
            
        }
        
        
        remove_modal_spinner()
    })
    
    
    
    ##### carga shape para mapas
    output$refMapasExport.name<- renderUI("Seleccione un archivo vectorial" )
    
    observeEvent(input$refMapasExport,{
        
        #print(input$shpMapaMod)
        show_modal_spinner()
        
        if( !is.null(input$refMapasExport) ){#, input$shpMapaMod!="", input$shpMapaMod!="---"
            
            for(fdir in 1:length(input$refMapasExport$datapath)  ){
                file.copy(input$refMapasExport$datapath[fdir], paste0(getwd(), "/tempFiles/", input$refMapasExport$name[fdir]) )   
            }
            
            tmpF<- list.files(paste0(getwd(), "/tempFiles"), full.names=T )
            
            tryout<-tryCatch(
                expr =  { shpMapExportMaps$df <- readOGR( tmpF[grepl(".shp$", tmpF)], verbose=F ) 
                }    
                ,error= function(e){ return("error") }  
                
            )
        }
        remove_modal_spinner() 
        s<-strsplit(tmpF[grepl(".shp$", tmpF)],"/")[[1]]
        
        if(class(tryout) %in% c("SpatialPolygonsDataFrame", "SpatialPointsDataFrame", "SpatialLinesDataFrame") ){
            output$refMapasExport.name<- renderUI(helpText(paste0("Referencia cartográfica: ",s[length(s)] )) )
            
            if(class(tryout) == "SpatialPolygonsDataFrame" ){
                shpMapExportMaps$df <- as(shpMapExportMaps$df , 'SpatialLinesDataFrame')
            }
            
            
            showModal( modalDialog(h2("¡Completado!"), #h4("No se puede cargar el archivo."),
                                   helpText(paste0("Se cargó con éxito el archivo: .../",s[length(s)] )),
                                   footer=modalButton("Cerrar"), size="m"))
        }else if(class(tryout) == "character"){
            output$refMapasExport.name<- renderUI(helpText("Seleccione un archivo vectorial") )
            showModal( modalDialog(h2("¡Error!"), h4("No se puede cargar el archivo."),
                                   helpText("Verifique que se hayan cargado al menos los archivo"),
                                   helpText("con terminación '.shp', '.dbf', '.prj' y '.shx' "),
                                   footer=modalButton("Cerrar"), size="m"))
            
        }else{
            output$refMapasExport.name<- renderUI("Seleccione un archivo vectorial" )
            showModal( modalDialog(h2("¡Error!"), h4("Seleccione un arcvhio vectorial de tipo poligonal"),
                                   footer=modalButton("Cerrar"), size="m"))
        }
        
        reset("refMapasExport")
        file.remove(tmpF)     
        
    })
    
    
    ##### desabilitador boton exportar mapas
    observeEvent(list(input$ResEnsamble, input$exportPNG, input$exportRaster, input$exportEnsamble, input$wd.out.new ),{
        if(all(!is.null(input$ResEnsamble$datapath), any(input$exportPNG, input$exportRaster), input$exportEnsamble!="---",any(is.null(input$wd.out.new), try(dir.exists(input$wd.out.new),silent=T)) )){
            enable("btnExportarMapas")
        }else{
            disable("btnExportarMapas")
        }
    })
    
    
    
    #####  
    
    
    ###### server modelacion #####  
    
    
    CovsMod<-reactiveValues(df=data.frame(Covariable=character(), Value=numeric()) )
    shpMap<-reactiveValues(df="")
    MapMod<-reactiveValues(df="")
    hipsIN<-reactiveValues(validHips="")
    output$mensajeShp<- renderUI("Seleccione un archivo vectorial" )
    #MapViewOutput<-reactiveValues(SP="")
    hipSimulada<- reactiveValues(sim="") 
    
    
    controlOBJ<-reactiveValues(df=0)
    
    ##### carga datos de covariables
    observeEvent(input$covsPrepMod,{
        
        if( !is.null( input$covsPrepMod$datapath ) ){
            show_modal_spinner()
            CovsMod$df <- readRDS( input$covsPrepMod$datapath )
            CovsMod$df$control<-rownames(CovsMod$df)
            
            MapMod$df<-CovsMod$df
            if( all(c("x", "y") %in% names(MapMod$df)) ){
                coordinates(MapMod$df)<-~x+y
                gridded(MapMod$df)<-T
                remove_modal_spinner()  
            }else{
                remove_modal_spinner()  
                
                output$errorCovsXY<- renderUI({
                    HTML(paste0(
                        "El archivo de covariables no contiene campos de coordenadas [x, y].<br/><br/>
                        Seleccione un archivo válido, se recomienda usar solo archivos generados <br/>
                        con la herramienta de Armonización cartográfica"
                    ))})
                
                showModal( modalDialog(h2("¡Error!"), 
                                       htmlOutput("errorCovsXY"),
                                       footer=modalButton("Cerrar"), size="m"))
                
            }
            
        }    
        
        
    })
    
    
    ##### carga shape para mapas
    observeEvent(input$shpMapaMod2,{
        
        #print(input$shpMapaMod2)
        show_modal_spinner()
        
        if( !is.null(input$shpMapaMod2) ){#, input$shpMapaMod2!="", input$shpMapaMod2!="---"
            
            for(fdir in 1:length(input$shpMapaMod2$datapath)  ){
                file.copy(input$shpMapaMod2$datapath[fdir], paste0(getwd(), "/tempFiles/", input$shpMapaMod2$name[fdir]) )   
            }
            
            tmpF<- list.files(paste0(getwd(), "/tempFiles"), full.names=T )
            
            tryout<-tryCatch(
                expr =  { shpMap$df <- readOGR( tmpF[grepl(".shp$", tmpF)], verbose=F ) 
                }    
                ,error= function(e){ return("error") }  
                
            )
        }
        remove_modal_spinner() 
        s<-strsplit(tmpF[grepl(".shp$", tmpF)],"/")[[1]]
        
        if(class(tryout) %in% c("SpatialPolygonsDataFrame", "SpatialPointsDataFrame", "SpatialLinesDataFrame") ){
            output$mensajeShp<- renderUI(helpText(paste0("Referencia cartográfica: ",s[length(s)] )) )
            
            if(class(tryout) == "SpatialPolygonsDataFrame" ){
                shpMap$df <- as(shpMap$df , 'SpatialLinesDataFrame')
            }
            
            
            showModal( modalDialog(h2("¡Completado!"), #h4("No se puede cargar el archivo."),
                                   helpText(paste0("Se cargó con éxito el archivo: .../",s[length(s)] )),
                                   footer=modalButton("Cerrar"), size="m"))
        }else if(class(tryout) == "character"){
            output$mensajeShp<- renderUI(helpText("Seleccione un archivo vectorial") )
            showModal( modalDialog(h2("¡Error!"), h4("No se puede cargar el archivo."),
                                   helpText("Verifique que se hayan cargado al menos los archivo"),
                                   helpText("con terminación '.shp', '.dbf', '.prj' y '.shx' "),
                                   footer=modalButton("Cerrar"), size="m"))
            
        }else{
            output$mensajeShp<- renderUI("Seleccione un archivo vectorial" )
            showModal( modalDialog(h2("¡Error!"), h4("Seleccione un arcvhio vectorial de tipo poligonal"),
                                   footer=modalButton("Cerrar"), size="m"))
        }
        
        reset("shpMapaMod2")
        file.remove(tmpF)     
        
    })
    
    
    
    #### crea drop de las hipotesis
    output$checkBoxGHipMod<- renderUI({
        
        if( all(input$DirCheckMod != "", !is.null(input$covsPrepMod$datapath), is.numeric(controlOBJ$df) ) ){
            if(dir.exists(input$DirCheckMod)){
                wd<-input$DirCheckMod 
                
                hips<-list.files(wd, pattern="Hipotesis", full.names = F, recursive = T)
                hips<-hips[grepl(".csv$", hips)]
                print(hips)
                #print(names(CovsMod$df))
                
                if(length(hips)>1){
                    validacion<-lapply(hips, validarFun, wd=wd, covVars=names(CovsMod$df))
                    validacion
                    
                    hipsIn <- hips[unlist(lapply(validacion, all))]
                    hipsOUT <- hips[!unlist(lapply(validacion, all))]
                    
                    hipsDONE <- hipsIn[unlist(lapply( paste0(wd, "/", gsub("Hipotesis","Resultados/Modelos/Modelo_Hipotesis",gsub(".csv",".tif",hipsIn)) ), file.exists ) )]
                    hipsIn <- hipsIn[!unlist(lapply( paste0(wd, "/", gsub("Hipotesis","Resultados/Modelos/Modelo_Hipotesis",gsub(".csv",".tif",hipsIn)) ), file.exists ) )]
                    
                    hipsIN$validHips<-hipsIn
                    
                    
                    output$mensajeModelos <- renderPrint(
                        if( all(length(hipsOUT)>0, length(na.omit(hipsDONE))>0 ) ){
                            cat("Hipótesis con errores de compatibilidad: ", paste0("\n",hipsOUT), "\n\nHipótesis ya procesadas (el modelo ya existe):", paste0("\n",hipsDONE))
                        }else if( all(length(hipsOUT)==0, length(na.omit(hipsDONE))>0 ) ){
                            cat("Hipótesis ya procesadas (el modelo ya existe):", paste0("\n",hipsDONE))
                        }else if( all(length(hipsOUT)>0, length(na.omit(hipsDONE))==0 ) ){
                            cat("Hipótesis con errores de compatibilidad: ", paste0("\n",hipsOUT))
                        }else{"Archivos de hipótesis modelados exitosamente"}
                    )   
                    
                    if(length(hipsIn)>=1){
                        selectInput("checkGroup2Mod", label = NULL, choices = c("Todas las hipótesis",hipsIn), selected = "Todas las hipótesis") 
                    }
                }else{
                    output$mensajeModelos <- renderPrint("")
                    helpText("Error: No se encontraron archivos de hipotesis")
                }
                
            }else{
                output$mensajeModelos <- renderPrint("")
                helpText("Error: El direcorio raíz no existe o no se puede tener acceso.")
            }
        }else{
            output$mensajeModelos <- renderPrint("")
            helpText("Seleccione un directorio raíz y un archivo de covariables")
        }
        
        
    })  
    
    
    
    #### crea drop de las hipotesis para la simulacion
    output$checkBoxGHipSim<- renderUI({
        
        if( all(input$DirCheckMod != "", !is.null(input$covsPrepMod$datapath), is.numeric(controlOBJ$df) ) ){
            if(dir.exists(input$DirCheckMod)){
                wd<-input$DirCheckMod 
                
                hipsSim<-list.files(wd, pattern="Hipotesis", full.names = F, recursive = T)
                hipsSim<-hipsSim[grepl(".csv$", hipsSim)]
                #print(hips)
                #print(names(CovsMod$df))
                
                if(length(hipsSim)>1){
                    validacionSim<-lapply(hipsSim, validarFun, wd=wd, covVars=names(CovsMod$df))
                    validacionSim
                    
                    hipsInSim <- hipsSim[unlist(lapply(validacionSim, all))]
                    
                    if(length(hipsInSim)>=1){
                        selectInput("checkGroup2ModSim", label = NULL, choices = c("---",hipsInSim), selected = "---") 
                    }
                    
                    
                }else{
                    helpText("Error: No se encontraron archivos de hipotesis")
                }
                
                
                
            }}
    })
    
    
    ###### disabilitador boton iniciar
    
    observeEvent(list(input$covsPrepMod, input$DirCheckMod, input$checkGroup2Mod, CovsMod$df),{
        if( all(input$DirCheckMod != "", !is.null(input$covsPrepMod$datapath), dir.exists(input$DirCheckMod), !is.null(input$checkGroup2Mod), all(c("x", "y") %in% names(CovsMod$df)) ) ){
            enable("runModelo")
        }else{
            disable("runModelo")
        }
    })
    
    
    ###### inicia la modelacion
    
    observeEvent(input$runModelo,{
        
        #print(hipsIN$validHips)
        
        if(input$checkGroup2Mod == "Todas las hipótesis"){hips <- hipsIN$validHips}else{hips <- input$checkGroup2Mod}
        
        #print("TEST")
        withProgress(message = "Modelando:", value = 0, {
            show_modal_spinner()
            
            
            for(d in 1:length(hips)){
                incProgress(1/length(hips), detail =  hips[d])
                
                print(hips[d])
                
                res<-CovsMod$df[,c("x","y")]
                #  wd<- paste0(input$DirCheckMod, "/", strsplit(hips[d], "/")[[1]][1]  )
                
                wd<-input$DirCheckMod
                
                ### crea carpetas de salida
                outDirs<- c(paste0(wd, "/", strsplit(hips[d], "/")[[1]][1],"/" ,"Resultados"), 
                            paste0(wd, "/",strsplit(hips[d], "/")[[1]][1],"/" , "Resultados/Estadisticas"), 
                            paste0(wd, "/",strsplit(hips[d], "/")[[1]][1],"/" , "Resultados/Graficas"), 
                            paste0(wd, "/",strsplit(hips[d], "/")[[1]][1],"/" , "Resultados/Modelos") )
                
                sapply(outDirs[!sapply(outDirs, dir.exists)], dir.create)
                
                log.tmp <- file.path( paste0(wd,"/",gsub("Hipotesis","Resultados/Modelos/Modelo_Hipotesis", gsub(".csv",".log", hips[d]))) )
                lf <- log_open(log.tmp, show_notes=F, logdir = F)
                
                
                hip<- read.csv(paste(wd,"/",hips[d], sep=""),sep=",",header=T)
                hip<-hip[,1:6]
                hip<-hip[which(hip$Estado==1),]
                
                names(hip)<-c("Tipo", "Nombre", "Sintaxis","Estado","PesoPositivo","PesoNegativo")
                
                hip$Sintaxis<-as.character(hip$Sintaxis)
                hip$Sintaxis<-toupper(hip$Sintaxis)
                
                
                log_print("-------- Hipótesis de entrada ---------", console =F)
                
                log_print(hip, console =F)
                
                
                hip$Nombre<-as.character(hip$Nombre)
                hip$Nombre<-gsub(" ","",hip$Nombre)
                
                shps<-hip$Nombre[hip$Tipo=="spp"]
                
                shpError<-""
                
                
                shps.tmp<-readOGR(paste0(wd,"/", strsplit(hips[d], "/")[[1]][1],"/",shps[1]), verbose=F )
                
                #print("Etiqueta" %in% names(shps.tmp))
                
                if( ("Etiqueta" %in% names(shps.tmp) ) ){
                    shps.tmp@data$shp<- shps[1]
                    shps.tmp<-shps.tmp[,c("shp","Etiqueta")]
                }else{
                    shps.tmp@data$Etiqueta<-"Error"
                    shps.tmp@data$shp<- shps[1]
                    shps.tmp<-shps.tmp[,c("shp","Etiqueta")]
                    shpError<-c(shpError, shps[1])
                    
                }
                
                
                if(length(shps)>1){
                    for(i in 2:length(shps)){
                        
                        #print(shps[i])
                        tmpSHP <- readOGR(paste0(wd,"/", strsplit(hips[d], "/")[[1]][1],"/", shps[i]), verbose=F )
                        
                        
                        if( ("Etiqueta" %in% names(tmpSHP) ) ){
                            tmpSHP@data$shp<- shps[i]
                            
                            tmpSHP<-tmpSHP[,c("shp","Etiqueta")]
                            shps.tmp<-rbind(shps.tmp, tmpSHP)
                            
                        }else{
                            
                            shpError<-c(shpError, shps[i])
                        }
                    }}
                
                if(length(shpError)>1){
                    log_print("Archivos vectoriales sin campo de Etiqueta:", console =F)
                    log_print(shpError[-1], console =F)
                }else{
                    log_print("Todos los archivos vectoriales se cargaron exitosamente", console =F)
                }
                
                shps.tmp<- shps.tmp[which(shps.tmp@data$Etiqueta !="Error"), ] ### remueve puntos de control para crear archivo
                
                shps.tmp<- shps.tmp[,c("shp")]
                
                shps.tmp$shp<- as.factor(shps.tmp$shp)
                
                log_print("Datos de entrenamiento introducidos:", console =F)
                
                log_print( tapply(shps.tmp@data$shp, shps.tmp@data$shp, length), console =F) 
                
                
                crs(shps.tmp)<-""
                crs(MapMod$df)<-""
                
                ov<- over(shps.tmp, MapMod$df)
                
                shps.tmp@data<-cbind(shps.tmp@data, ov)
                
                if(input$rmDupMod){
                    log_print("Datos de entrenamiento depurados por presencias:", console =F)
                    shps.tmp<-shps.tmp[!duplicated(shps.tmp@data$control),] 
                    log_print( tapply(shps.tmp@data$shp, shps.tmp@data$shp, length), console =F)
                }else{
                    log_print("El usuario decidió no remover los datos duplicados", console =F)
                    
                }
                
                
                
                
                
                ################# Atributos categoricos
                
                hcat<-hip$Nombre[hip$Tipo=="factor"]
                
                
                if(length(hcat)>0){
                    
                    log_print("Factores categóricos:", console =F)
                    log_print(hcat, console =F)
                    
                    for(c in 1:length(hcat)){
                        
                        tmp_ov <- cbind(shp=shps.tmp@data$shp , shps.tmp@data[hcat[c]])
                        #str(tmp_ov)
                        
                        tmp_ov[hcat[c]]<-as.factor(tmp_ov[,hcat[c]])
                        tmp_ov<-na.omit(tmp_ov)
                        
                        
                        catw <- data.frame(Especie=levels(tmp_ov$shp))
                        catw[levels(tmp_ov[,hcat[c]])]<-NA
                        
                        for(csp in 1:dim(catw)[1]){
                            tmp<-tmp_ov[which(tmp_ov$shp == catw$Especie[csp] ),]
                            catw[csp,2:dim(catw)[2]]<-tapply(tmp[,hcat[c]], tmp[,hcat[c]], length )
                        }### fin for de csp
                        
                        
                        #str(catw)
                        catw$Especie<-as.character(catw$Especie)
                        
                        catw <- rbind(catw,c("Suma", tapply(tmp_ov[,hcat[c]], tmp_ov[,hcat[c]], length) ))
                        catw <- rbind(catw,c("Pesos", round(tapply(tmp_ov[,hcat[c]], tmp_ov[,hcat[c]], length)*100/dim(tmp_ov)[1],2) ))
                        
                        catw[is.na(catw)]<-""
                        log_print(paste("Variable: ", hcat[c],sep=""), console =F)
                        log_print("Estadística para factores categóricos:", console =F)
                        log_print(catw, console =F)
                        
                        #sum(as.numeric(catw[dim(catw)[1],2:dim(catw)[2]]), na.rm=T)
                        
                        assign(hcat[c], catw)
                        
                        #hcat[c]
                        
                        w <- data.frame(Clas=names(catw)[2:dim(catw)[2]], w=as.numeric(catw[dim(catw)[1],2:dim(catw)[2]])/100) 
                        
                        log_print("Pesos:", console =F)
                        log_print(w, console =F)
                        
                        res[,hcat[c]]<-NA
                        #str(res)
                        
                        for(p in 1:dim(w)[1]){
                            
                            res[which(CovsMod$df[,hcat[c]] == w$Clas[p]),hcat[c]] <- w$w[p]
                            
                        }### fin for asigan pesos
                        
                        
                    }##### fin for vars categoricas
                    
                }else{
                    log_print("--------------------------Sin factores categóricos--------------------", console =F)
                }#### fin if si hay variables categoricas
                
                
                
                
                ################# Atributos continuos
                
                hcon<-hip[hip$Tipo %in% c("integer", "numeric"),]
                
                #print(str(hcon))
                #print(hcon)
                
                if(dim(hcon)[1]>0){
                    
                    
                    #hcon
                    #str(hcon)
                    hcon$Sintaxis<-as.character(hcon$Sintaxis)
                    
                    log_print("Factores continuos:", console =F)
                    log_print(hcon$Nombre, console =F)
                    
                    #tmp_ov <- over(data, nac.geo)
                    
                    tmp_ov <- shps.tmp@data[,hcon$Nombre]
                    
                    # print(str(tmp_ov))
                    
                    dimnames=names(tmp_ov)[names(tmp_ov) %in% hcon$Nombre]
                    tmp_ov <- tmp_ov[,names(tmp_ov) %in% hcon$Nombre]
                    tmp_ov <-as.data.frame(tmp_ov)
                    names(tmp_ov)= dimnames
                    
                    
                    contw <- data.frame(Variable=names(tmp_ov) )
                    contw[c("Min","Q1","Mediana","Media","Q3","Max","SD")]<-NA
                    
                    log_print("Estadística para factores continuos:", console =F)
                    #contw
                    
                    for(v in 1:dim(contw)[1]){
                        
                        
                        contw[v,2]<-round( min(tmp_ov[,v], na.rm = T) ,3)
                        contw[v,3]<-round(quantile(tmp_ov[,v], na.rm = T)[2] ,3)
                        contw[v,4]<-round( median(tmp_ov[,v], na.rm = T) ,3)
                        contw[v,5]<-round( mean(tmp_ov[,v], na.rm = T) ,3)
                        contw[v,6]<-round(quantile(tmp_ov[,v], na.rm = T)[4] ,3)
                        contw[v,7]<-round( max(tmp_ov[,v], na.rm = T) ,3)
                        contw[v,8]<-round( sd(tmp_ov[,v], na.rm = T) ,3)
                        
                        
                    }### fin for de v, stats de continuas
                    
                    
                    assign("StatsContinuas" , contw)
                    log_print(StatsContinuas, console =F)
                    
                    s <- strsplit(hcon$Sintaxis, "/")
                    
                    for(ss in 1:dim(hcon)[1]){
                        hcon$N[ss]<-length(s[[ss]])
                        
                    }
                    
                    hcon$N<-as.numeric(hcon$N)
                    
                    for(i in 1:dim(hcon)[1]){
                        
                        #hcon
                        
                        hcon$Sintaxis[i]<-gsub("MIN",contw[contw$Variable == hcon$Nombre[i],]$Min, hcon$Sintaxis[i])
                        hcon$Sintaxis[i]<-gsub("Q1",contw[contw$Variable == hcon$Nombre[i],]$Q1, hcon$Sintaxis[i])
                        hcon$Sintaxis[i]<-gsub("MEDIANA",contw[contw$Variable == hcon$Nombre[i],]$Mediana, hcon$Sintaxis[i])
                        hcon$Sintaxis[i]<-gsub("MEDIA",contw[contw$Variable == hcon$Nombre[i],]$Media, hcon$Sintaxis[i])
                        hcon$Sintaxis[i]<-gsub("Q3",contw[contw$Variable == hcon$Nombre[i],]$Q3, hcon$Sintaxis[i])
                        hcon$Sintaxis[i]<-gsub("MAX",contw[contw$Variable == hcon$Nombre[i],]$Max, hcon$Sintaxis[i])
                        hcon$Sintaxis[i]<-gsub("SD",contw[contw$Variable == hcon$Nombre[i],]$SD, hcon$Sintaxis[i])
                        
                    }
                    
                    
                    
                    for(i in 1:dim(hcon)[1]){
                        
                        if(hcon$N[i]==1){
                            
                            log_print(paste("Variable: ", hcon$Nombre[i],sep=""), console =F)
                            
                            w.2 <- as.numeric(strsplit(hcon$Sintaxis[i], ":")[[1]])
                            
                            log_print(paste("Rango de ",w.2[1], " a ", w.2[2], sep=""), console =F)
                            
                            res[hcon$Nombre[i]] <- hcon$PesoNegativo[i] #NA
                            
                            tmp <- CovsMod$df[hcon$Nombre[i]]
                            
                            res[hcon$Nombre[i]][ tmp >= w.2[1] & tmp <= w.2[2] ]<- hcon$PesoPositivo[i] #0.5
                            
                            # res[hcon$Nombre[i]][ tmp >= contw[contw$Variable[contw$Variable==hcon$Nombre[i]],"Min"] & tmp < contw[contw$Variable[contw$Variable==hcon$Nombre[i]],"Q1"] ]<- 0.25
                            # res[hcon$Nombre[i]][ tmp > contw[contw$Variable[contw$Variable==hcon$Nombre[i]],"Q3"] & tmp <= contw[contw$Variable[contw$Variable==hcon$Nombre[i]],"Max"] ]<- 0.25
                            
                            
                        }else{#### else para cuando es una condicion multiple
                            
                            
                            log_print(paste("Variable: ", hcon$Nombre[i],sep=""), console =F)
                            conds<-strsplit(hcon$Sintaxis[i], "/")[[1]]
                            
                            res[hcon$Nombre[i]] <- hcon$PesoNegativo[i] #NA
                            
                            tmp<- CovsMod$df[hcon$Nombre[i]]
                            
                            for(j in 1:hcon$N[i]){
                                
                                w.2 <- as.numeric(strsplit(conds[j], ":")[[1]])
                                w.2
                                
                                log_print(paste("Rango de ",w.2[1], " a ", w.2[2], sep=""), console =F)
                                
                                res[hcon$Nombre[i]][ tmp >= w.2[1] & tmp <= w.2[2] ]<- hcon$PesoPositivo[i] #0.5
                                
                                
                            }#### fin for de j
                        }#### fin if
                        
                        
                    }#### fin for de i
                    
                    
                    
                }else{
                    log_print("--------------------------Sin factores continuos--------------------", console =F)
                }#### fin if si hay variables continuas
                
                
                
                # print( str(res) )
                
                
                ####################### suma final
                
                res$Z<-apply(res[3:dim(res)[2]],1,sum, na.rm=T)
                
                map<-res[,c("x","y","Z")]
                coordinates(map)<-~x+y
                gridded(map)<-T
                r<-raster(map)
                
                r[r==0]<-NA
                
                
                writeRaster(r, paste0(wd,"/",gsub("Hipotesis","Resultados/Modelos/Modelo_Hipotesis", gsub(".csv",".tif", hips[d]))) , overwrite=F)
                
                
                map<-res[,c("x","y","Z")]
                
                #val <- dim(contw)[1]*.5 + length(hcat)*1
                #val<- sum(hip$PesoPositivo, na.rm=T) 
                val<- sum(hcon$PesoPositivo, na.rm=T) + length(hcat)
                
                
                #print(paste0("VALOR: ",val))
                
                map$Z<- cut(map$Z, breaks=c(seq(0,val, (val/10))), labels=c(1:10))
                
                coordinates(map)<-~x+y
                gridded(map)<-T
                rr<-raster(map)
                
                writeRaster(rr, paste0(wd,"/",gsub("Hipotesis","Resultados/Modelos/Modelo_Hipotesis", gsub(".csv","_Clases.tif", hips[d]))) , overwrite=F)
                
                valp<-data.frame(Rango=paste(seq(0,val, (val/10))[-11], seq(0,val, (val/10))[-1],sep=" - "), Clase=1:10)
                
                
                #print(class(shpMap$df))
                
                if( class(shpMap$df) %in% c("SpatialLinesDataFrame", "SpatialPointsDataFrame") ){
                    
                    png(paste0(wd,"/",gsub("Hipotesis","Resultados/Modelos/Modelo_Hipotesis", gsub(".csv","_Clases.png", hips[d]))), width = 480*3, height = 480*3, pointsize = 18)
                    plot(rr, breaks=seq(0,10, 1), col=topo.colors(10), main= paste0("Modelo para archivo ", hips[d]) )
                    plot(shpMap$df, add=T, col="black", pch=19)
                    legend("topright", legend=paste0(valp$Clase[cellStats(rr, min):cellStats(rr, max)], ": ",valp$Rango[cellStats(rr, min):cellStats(rr, max)] ,sep=""), bty="n")
                    dev.off()
                    
                }else{
                    png(paste0(wd,"/",gsub("Hipotesis","Resultados/Modelos/Modelo_Hipotesis", gsub(".csv","_Clases.png", hips[d]))), width = 480*3, height = 480*3, pointsize = 18)
                    plot(rr, breaks=seq(0,10, 1), col=topo.colors(10), main= paste0("Modelo para archivo ", hips[d]) )
                    legend("topright", legend=paste0(valp$Clase[cellStats(rr, min):cellStats(rr, max)], ": ",valp$Rango[cellStats(rr, min):cellStats(rr, max)] ,sep=""), bty="n")
                    dev.off()  
                }
                
                
                
                log_print("Rasters creados:", console =F)
                log_print(paste0(wd,"/",gsub("Hipotesis","Resultados/Modelos/Modelo_Hipotesis", gsub(".csv",".tif", hips[d]))) , console =F)
                log_print(paste0(wd,"/",gsub("Hipotesis","Resultados/Modelos/Modelo_Hipotesis", gsub(".csv","_Clases.tif", hips[d]))) , console =F)
                
                
                log_print("Estadística básica del resultado final:", console =F)
                log_print(summary(res$Z), console =F)
                
                log_print("Clases:", console =F)
                log_print(valp, console =F)
                
                
                log_close()
                #save(lf, file=paste0(wd,"/",gsub("Hipotesis","Resultados/Modelos/Modelo_Hipotesis", gsub(".csv",".rds", hips[d]))) )
                
                
                #print("cerrado")
            }### fin de for de hips
            
        })##### fin barra de progreso
        
        controlOBJ$df<-controlOBJ$df+1
        remove_modal_spinner()
        
        
    })
    
    
    
    #############  simulacion 
    
    
    
    ################# tabla hipotesis simulacion
    react_valsSim <- reactiveValues(
        df = data.frame(Tipo=character(), Nombre=character(), Sintaxis=character(), Estado=character(), PesoPositivo=numeric(), PesoNegativo=numeric(), stringsAsFactors=F ),
        dt_row = NULL,
        add_or_edit = NULL,
        edit_button = NULL,
        keep_track_id = nrow(df) + 1
    )
    
    ##### crea tabla con el contenido reactivo de react_vals
    output$HipotesisTablaSimulacion <- renderDataTable(
        {
            isolate(react_valsSim$df)
        },
        escape = F, 
        rownames = FALSE,  caption = tags$caption(style="caption-side: top; text-align: left; margin: 8px 5;","Tabla 1. Hipótesis de salida"),
        options = list(processing = FALSE, ordering=F, paging = T,  language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'), pageLength = 5,
                       headerCallback = JS(
                           "function( thead, data, start, end, display ) {
      $(thead).closest('thead').find('th').css('background-color', '#7A1F1A');
              }"),
      initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'color': 'white'});",
          "}"
      ), autoWidth = F, scrollX=T
      
        ),
      editable = list(target = "cell", disable = list(columns =c(0:1)))
    )
    
    #### proxy de tabla para el manejo interno 
    proxySim <- dataTableProxy("HipotesisTablaSimulacion") 
    observe({
        replaceData(proxySim, react_valsSim$df, resetPaging = FALSE, rownames = FALSE)
    })
    
    #### rellena tabla de simulacion
    
    observeEvent(input$checkGroup2ModSim,{
        if(all( input$checkGroup2ModSim != "---" )){
            wd<-input$DirCheckMod
            tmptable<- read.csv(paste(wd,"/",input$checkGroup2ModSim, sep=""),sep=",",header=T)
            tmptable$Sintaxis<-as.character(tmptable$Sintaxis)
            react_valsSim$df<-tmptable[,1:6]
        }else{
            react_valsSim$df <- data.frame(Tipo=character(), Nombre=character(), Sintaxis=character(), Estado=character(), PesoPositivo=numeric(), PesoNegativo=numeric(), stringsAsFactors=F )
        }
    })
    
    
    ######## evento para la actualizacion de edicion de celda
    observeEvent(input$HipotesisTablaSimulacion_cell_edit,{
        ind = input$HipotesisTablaSimulacion_cell_edit
        #str(info)
        i = ind$row
        j = ind$col+1
        z = ind$value
        # print(paste0(i,"-",j,"-",z))
        react_valsSim$df[i, j] <- isolate(z)
    })
    
    
    #### desabilita botones de simulacion
    observeEvent(list(input$checkGroup2ModSim, input$SufijoSimulacion ),{
        
        if(all( input$checkGroup2ModSim != "---", input$SufijoSimulacion!="" )){
            enable("bttnSaveSimHip")
        }else{
            disable("bttnSaveSimHip")
        }
        
        if( all(input$checkGroup2ModSim != "---", !is.null(input$checkGroup2ModSim) ) ){
            enable("bttnSimulacion")
        }else{
            disable("bttnSimulacion")
            #output$ModSimulacion <- renderPlot({plot.new()})
        }
        
    })
    
    
    
    
    
    
    ######## aplica cambios de la hipotesis al mapa
    observeEvent(input$bttnSimulacion,{
        #print(react_valsSim$df)
        
        show_modal_spinner()
        
        print(input$checkGroup2ModSim)
        
        wd<-input$DirCheckMod
        
        res<-CovsMod$df[,c("x","y")]
        
        hip<- react_valsSim$df
        
        hipSimulada$tabla<-hip
        
        hip<-hip[which(hip$Estado==1),]
        
        
        
        # print(hip)
        
        names(hip)<-c("Tipo", "Nombre", "Sintaxis","Estado","PesoPositivo","PesoNegativo")
        
        hip$Sintaxis<-as.character(hip$Sintaxis)
        hip$Sintaxis<-toupper(hip$Sintaxis)
        
        hip$PesoPositivo<-as.numeric(hip$PesoPositivo)
        hip$PesoNegativo<-as.numeric(hip$PesoNegativo)
        
        
        
        hip$Nombre<-as.character(hip$Nombre)
        hip$Nombre<-gsub(" ","",hip$Nombre)
        
        shps<-hip$Nombre[hip$Tipo=="spp"]
        shpError<-""
        
        shps.tmp<-readOGR(paste0(wd,"/", strsplit(input$checkGroup2ModSim, "/")[[1]][1],"/",shps[1]), verbose=F )
        
        #print("Etiqueta" %in% names(shps.tmp))
        
        if( ("Etiqueta" %in% names(shps.tmp) ) ){
            shps.tmp@data$shp<- shps[1]
            shps.tmp<-shps.tmp[,c("shp","Etiqueta")]
        }else{
            shps.tmp@data$Etiqueta<-"Error"
            shps.tmp@data$shp<- shps[1]
            shps.tmp<-shps.tmp[,c("shp","Etiqueta")]
            shpError<-c(shpError, shps[1])
            
        }
        
        
        if(length(shps)>1){
            for(i in 2:length(shps)){
                
                #print(shps[i])
                tmpSHP <- readOGR(paste0(wd,"/", strsplit(input$checkGroup2ModSim, "/")[[1]][1],"/", shps[i]), verbose=F )
                
                
                if( ("Etiqueta" %in% names(tmpSHP) ) ){
                    tmpSHP@data$shp<- shps[i]
                    
                    tmpSHP<-tmpSHP[,c("shp","Etiqueta")]
                    shps.tmp<-rbind(shps.tmp, tmpSHP)
                    
                }else{
                    
                    shpError<-c(shpError, shps[i])
                }
            }}
        
        
        
        shps.tmp<- shps.tmp[which(shps.tmp@data$Etiqueta !="Error"), ] ### remueve puntos de control para crear archivo
        
        shps.tmp<- shps.tmp[,c("shp")]
        
        shps.tmp$shp<- as.factor(shps.tmp$shp)
        
        crs(shps.tmp)<-""
        crs(MapMod$df)<-""
        
        ov<- over(shps.tmp, MapMod$df)
        
        shps.tmp@data<-cbind(shps.tmp@data, ov)
        
        if(input$rmDupMod){
            shps.tmp<-shps.tmp[!duplicated(shps.tmp@data$control),] 
            print(dim(shps.tmp@data))
        }else{
            shps.tmp<-shps.tmp
            print(dim(shps.tmp@data))
        }
        
        
        
        
        print("categoricas")
        
        ################# Atributos categoricos
        
        hcat<-hip$Nombre[hip$Tipo=="factor"]
        
        
        if(length(hcat)>0){
            
            for(c in 1:length(hcat)){
                
                tmp_ov <- cbind(shp=shps.tmp@data$shp , shps.tmp@data[hcat[c]])
                #str(tmp_ov)
                
                tmp_ov[hcat[c]]<-as.factor(tmp_ov[,hcat[c]])
                tmp_ov<-na.omit(tmp_ov)
                
                
                catw <- data.frame(Especie=levels(tmp_ov$shp))
                catw[levels(tmp_ov[,hcat[c]])]<-NA
                
                for(csp in 1:dim(catw)[1]){
                    tmp<-tmp_ov[which(tmp_ov$shp == catw$Especie[csp] ),]
                    catw[csp,2:dim(catw)[2]]<-tapply(tmp[,hcat[c]], tmp[,hcat[c]], length )
                }### fin for de csp
                
                
                #str(catw)
                catw$Especie<-as.character(catw$Especie)
                
                catw <- rbind(catw,c("Suma", tapply(tmp_ov[,hcat[c]], tmp_ov[,hcat[c]], length) ))
                catw <- rbind(catw,c("Pesos", round(tapply(tmp_ov[,hcat[c]], tmp_ov[,hcat[c]], length)*100/dim(tmp_ov)[1],2) ))
                
                catw[is.na(catw)]<-""
                
                #sum(as.numeric(catw[dim(catw)[1],2:dim(catw)[2]]), na.rm=T)
                
                assign(hcat[c], catw)
                
                #hcat[c]
                
                w <- data.frame(Clas=names(catw)[2:dim(catw)[2]], w=as.numeric(catw[dim(catw)[1],2:dim(catw)[2]])/100) 
                
                
                res[,hcat[c]]<-NA
                #str(res)
                
                for(p in 1:dim(w)[1]){
                    
                    res[which(CovsMod$df[,hcat[c]] == w$Clas[p]),hcat[c]] <- w$w[p]
                    
                }### fin for asigan pesos
                
                
            }##### fin for vars categoricas
            
        }
        
        
        
        ################# Atributos continuos
        
        hcon<-hip[hip$Tipo %in% c("integer", "numeric"),]
        
        
        if(dim(hcon)[1]>0){
            
            
            hcon$Sintaxis<-as.character(hcon$Sintaxis)
            
            
            #tmp_ov <- over(data, nac.geo)
            
            tmp_ov <- shps.tmp@data[,hcon$Nombre]
            
            # print(str(tmp_ov))
            
            dimnames=names(tmp_ov)[names(tmp_ov) %in% hcon$Nombre]
            tmp_ov <- tmp_ov[,names(tmp_ov) %in% hcon$Nombre]
            tmp_ov <-as.data.frame(tmp_ov)
            names(tmp_ov)= dimnames
            
            
            contw <- data.frame(Variable=names(tmp_ov) )
            contw[c("Min","Q1","Mediana","Media","Q3","Max","SD")]<-NA
            
            
            for(v in 1:dim(contw)[1]){
                
                
                contw[v,2]<-round( min(tmp_ov[,v], na.rm = T) ,3)
                contw[v,3]<-round(quantile(tmp_ov[,v], na.rm = T)[2] ,3)
                contw[v,4]<-round( median(tmp_ov[,v], na.rm = T) ,3)
                contw[v,5]<-round( mean(tmp_ov[,v], na.rm = T) ,3)
                contw[v,6]<-round(quantile(tmp_ov[,v], na.rm = T)[4] ,3)
                contw[v,7]<-round( max(tmp_ov[,v], na.rm = T) ,3)
                contw[v,8]<-round( sd(tmp_ov[,v], na.rm = T) ,3)
                
                
            }### fin for de v, stats de continuas
            
            
            assign("StatsContinuas" , contw)
            
            s <- strsplit(hcon$Sintaxis, "/")
            
            for(ss in 1:dim(hcon)[1]){
                hcon$N[ss]<-length(s[[ss]])
                
            }
            
            hcon$N<-as.numeric(hcon$N)
            
            for(i in 1:dim(hcon)[1]){
                
                #hcon
                
                hcon$Sintaxis[i]<-gsub("MIN",contw[contw$Variable == hcon$Nombre[i],]$Min, hcon$Sintaxis[i])
                hcon$Sintaxis[i]<-gsub("Q1",contw[contw$Variable == hcon$Nombre[i],]$Q1, hcon$Sintaxis[i])
                hcon$Sintaxis[i]<-gsub("MEDIANA",contw[contw$Variable == hcon$Nombre[i],]$Mediana, hcon$Sintaxis[i])
                hcon$Sintaxis[i]<-gsub("MEDIA",contw[contw$Variable == hcon$Nombre[i],]$Media, hcon$Sintaxis[i])
                hcon$Sintaxis[i]<-gsub("Q3",contw[contw$Variable == hcon$Nombre[i],]$Q3, hcon$Sintaxis[i])
                hcon$Sintaxis[i]<-gsub("MAX",contw[contw$Variable == hcon$Nombre[i],]$Max, hcon$Sintaxis[i])
                hcon$Sintaxis[i]<-gsub("SD",contw[contw$Variable == hcon$Nombre[i],]$SD, hcon$Sintaxis[i])
                
            }
            
            
            
            for(i in 1:dim(hcon)[1]){
                
                if(hcon$N[i]==1){
                    
                    
                    w.2 <- as.numeric(strsplit(hcon$Sintaxis[i], ":")[[1]])
                    
                    res[hcon$Nombre[i]] <- hcon$PesoNegativo[i] #NA
                    
                    tmp <- CovsMod$df[hcon$Nombre[i]]
                    
                    res[hcon$Nombre[i]][ tmp >= w.2[1] & tmp <= w.2[2] ]<- hcon$PesoPositivo[i] #0.5
                    
                    # res[hcon$Nombre[i]][ tmp >= contw[contw$Variable[contw$Variable==hcon$Nombre[i]],"Min"] & tmp < contw[contw$Variable[contw$Variable==hcon$Nombre[i]],"Q1"] ]<- 0.25
                    # res[hcon$Nombre[i]][ tmp > contw[contw$Variable[contw$Variable==hcon$Nombre[i]],"Q3"] & tmp <= contw[contw$Variable[contw$Variable==hcon$Nombre[i]],"Max"] ]<- 0.25
                    
                    
                }else{#### else para cuando es una condicion multiple
                    
                    
                    conds<-strsplit(hcon$Sintaxis[i], "/")[[1]]
                    
                    res[hcon$Nombre[i]] <- hcon$PesoNegativo[i] #NA
                    
                    tmp<- CovsMod$df[hcon$Nombre[i]]
                    
                    for(j in 1:hcon$N[i]){
                        
                        w.2 <- as.numeric(strsplit(conds[j], ":")[[1]])
                        w.2
                        
                        
                        res[hcon$Nombre[i]][ tmp >= w.2[1] & tmp <= w.2[2] ]<- hcon$PesoPositivo[i] #0.5
                        
                        
                    }#### fin for de j
                }#### fin if
                
                
            }#### fin for de i
            
            
            
        }
        
        
        ####################### suma final
        
        res$Z<-apply(res[3:dim(res)[2]],1,sum, na.rm=T)
        
        map<-res[,c("x","y","Z")]
        coordinates(map)<-~x+y
        gridded(map)<-T
        r<-raster(map)
        
        r[r==0]<-NA
        
        
        map<-res[,c("x","y","Z")]
        
        #val <- dim(contw)[1]*.5 + length(hcat)*1
        #val<- sum(hip$PesoPositivo, na.rm=T) 
        val<- sum(hcon$PesoPositivo, na.rm=T) + length(hcat)
        
        
        print(paste0("VALOR: ",val))
        
        map$Z<- cut(map$Z, breaks=c(seq(0,val, (val/10))), labels=c(1:10))
        
        coordinates(map)<-~x+y
        gridded(map)<-T
        rr<-raster(map)
        
        valp<-data.frame(Rango=paste(seq(0,val, (val/10))[-11], seq(0,val, (val/10))[-1],sep=" - "), Clase=1:10)
        
        
        output$ModSimulacion <- renderPlot({
            
            
            
            hipSimulada$sim<- isolate(input$checkGroup2ModSim)   #### guarda nombre de la simulacion para salvar sin errores 
            
            #plot(shps.tmp)
            
            if( class(shpMap$df) %in% c("SpatialLinesDataFrame", "SpatialPointsDataFrame") ){
                
                plot(rr, breaks=seq(0,10, 1), col=topo.colors(10), main= paste0("Simulación con base en ", isolate(input$checkGroup2ModSim )) )
                plot(shpMap$df, add=T, col="black", pch=19)
                legend("topright", legend=paste0(valp$Clase[cellStats(rr, min):cellStats(rr, max)], ": ",valp$Rango[cellStats(rr, min):cellStats(rr, max)] ,sep=""), bty="n")
                
            }else{
                plot(rr, breaks=seq(0,10, 1), col=topo.colors(10), main= paste0("Simulación con base en ", isolate(input$checkGroup2ModSim) ) )
                legend("topright", legend=paste0(valp$Clase[cellStats(rr, min):cellStats(rr, max)], ": ",valp$Rango[cellStats(rr, min):cellStats(rr, max)] ,sep=""), bty="n")
            }
            
            
            
        }, height = 800)
        
        remove_modal_spinner()
        
    })
    
    
    ####### salva hipotesis modificada
    
    observeEvent(input$bttnSaveSimHip,{
        wd<-input$DirCheckMod
        
        #print(hipSimulada$sim)
        
        #print(hipSimulada$tabla)
        
        #print(  paste0(wd,"/",gsub(".csv",paste0(input$SufijoSimulacion,".csv"), input$checkGroup2ModSim) ) )
        
        write.table(hipSimulada$tabla, paste0(wd,"/",gsub(".csv",paste0(input$SufijoSimulacion,".csv"), hipSimulada$sim) ), col.names=T, row.names = F, sep=",")
        controlOBJ$df<-controlOBJ$df+1
        
    })
    
    

    ######
    
    
    ####### server PCA #####
    
    options(ggrepel.max.overlaps = Inf)
    
    
    datosPCA<- reactiveValues(df="")
    res<-reactiveValues(PCA="")
    PCAcols<-reactiveValues(cols="")
    
    ##### inicializa la vista
    output$VarsPCAFact<- renderUI(
      disabled(selectInput("clasesVarPCA", "Variable de segmentación:", choices = c("Todos los datos"), selected = "Todos los datos", width = "100%"))
    ) 
    
    output$levelsVarsPCAFact<- renderUI(
      disabled(selectInput("VarLevelsPCA", "Clase:", choices = c("---"), selected = "---", width = "100%") )
    ) 
    
    output$PCAdimX<- renderUI(
      disabled(selectInput("grafPCA_X", label = "Eje x", choices = "---") )
    )
    
    output$PCAdimY<- renderUI(
      disabled(selectInput("grafPCA_Y", label = "Eje y", choices= "---"))
    )
    
    
    ##### carga los datos
    observeEvent(input$dataPCA,{
      
      if( !is.null( input$dataPCA$datapath ) ){
        
        show_modal_spinner()
        
        datosPCA$df<- read.table(input$dataPCA$datapath, header=T, sep=",", stringsAsFactors=T)
        datosPCA$df<- na.omit(datosPCA$df)
        
        # print(str(datosPCA$df))
        
        varsPCAtmp<-names(datosPCA$df)[lapply(datosPCA$df,class)!="factor"]
        
        varLog<-apply(datosPCA$df[,varsPCAtmp],2,var)!=0
        
        
        FactvarsPCAtmp<-names(datosPCA$df)[lapply(datosPCA$df,class)=="factor"]
        
        output$VarsPCAcheckbox<- renderUI(
          checkboxGroupInput("varsPCA", NULL ,choices = varsPCAtmp, selected = varsPCAtmp) 
        ) 
        
        output$VarsPCAFactVars<- renderUI(
          disabled(selectInput("clasesVarPCA", "Variable de segmentación:", choices = c("Todos los datos", FactvarsPCAtmp), selected = "Todos los datos", width = "100%"))
        )
        
        enable("dimsPCA")
        enable("saveSummaryPCA")
        
        
      }
      
      
      #print( length(names(datosPCA$df)[sapply(datosPCA$df, class) == "factor"]) )
      
      
      if(length(names(datosPCA$df)[sapply(datosPCA$df, class) == "factor"]) > 0){
        
        output$VarsPCAFact<- renderUI(
          disabled(selectInput("clasesVarPCA", "Variable de segmentación:", choices = c("Todos los datos", names(datosPCA$df)[sapply(datosPCA$df, class) == "factor"]), selected = "Todos los datos", width = "100%"))
        ) 
        
      }else{
        
        output$VarsPCAFact<- renderUI(
          disabled(selectInput("clasesVarPCA", "Variable de segmentación:", choices = c("Todos los datos"), selected = "Todos los datos", width = "100%"))
        ) 
      }
      
      
      
      
      remove_modal_spinner()
      
      if(!all(varLog)){
        
        #varsPCAtmp<-names(datosPCA$df[,varsPCAtmp])[varLog]
        varsPCAtmp<-names(varLog)[varLog]
        
        output$helpTextPCA<-renderUI( HTML(pasteFit(names(varLog)[!varLog], ", \n", ) ) )
        
        showModal( modalDialog(h2("¡Advertencia!"), helpText("Algunas variables tienen varianza igial a 0, no serán usadas para el cálculo. Revise:\n") ,
                               htmlOutput("helpTextPCA"),
                               footer=modalButton("Cerrar"), size="m", ))
        
      }
      
      
    })
    
    
    
    ###### calcular los pca
    
    observeEvent(input$btn_runPCA,{
      
      show_modal_spinner()
      
      dat1<- datosPCA$df[,input$varsPCA] ### los PCA solo trabajan con datos continuos.
      #print(str(dat1))
      
      dat2<- datosPCA$df[,names(datosPCA$df)[sapply(datosPCA$df, class) == "factor"]] ### para el ploteo segmentado por clases.
      
      ##### Estimacion del mejor numero de PCA's.
      if(input$dimsPCA){
        n<- estim_ncp(dat1, ncp.min = 2, ncp.max = dim(dat1)[2], scale = T, method = "GCV") 
        n<- n$ncp
      }else{
        n<- input$dimsPCAmanual
      }
      
      
      ##### Calcula los PCA
      res$PCA = PCA(dat1, scale.unit = T, ncp = n,   graph = F)
      #summary(res$PCA)
      
      
      ### Para graficas por clases 
      
      p<- cbind(as.data.frame(res$PCA$ind$coord), dat2) ### objeto de coordenadas por dimension 
      tmp.res<- cbind(datosPCA$df, as.data.frame(res$PCA$ind$coord)) 
      # print(str(p))
      
      output$PCAdimX<- renderUI(
        selectInput("grafPCA_X", label = "Eje x", choices = names(p)[grepl("Dim",names(p))], selected = names(p)[grepl("Dim",names(p))][1] ) 
      )
      
      output$PCAdimY<- renderUI(
        selectInput("grafPCA_Y", label = "Eje y", choices= names(p)[grepl("Dim",names(p))], selected = names(p)[grepl("Dim",names(p))][2] )
      )
      
      output$PCAsummary<- renderPrint(
        
        summary(res$PCA)
        
      )
      
      updateTabsetPanel(session, "PCAtabs",
                        selected = "Resumen")
      
      
      
      #### guarda el PCA
      
      if (input$saveSummaryPCA){
        
        if(dir.exists( input$outDirPCA )){
          
          log.tmp <- file.path( paste0(input$outDirPCA,"/", "PCA_Resumen.log" ) )
          lf <- log_open(log.tmp, show_notes=F, logdir = F)
          
          log_print("------- Archivo creado ------", console = F)
          
          log_print(paste0(input$outDirPCA,"/", "PCA_Resumen.log" ), console = F )
          log_print(paste0(input$outDirPCA,"/", "PCA_Resultado.csv" ), console = F )
          
          
          log_print("------- Variables dse entrada ------", console = F)
          log_print(input$varsPCA, console = F)
          
          log_print("------- Resumen ------", console = F)
          log_print(summary(res$PCA), console = F)
          
          
          log_close()
          
          
          write.table(tmp.res, paste0(input$outDirPCA,"/", "PCA_Resultado.csv" ), sep=",", col.names = T, row.names = F)
          
        }else{
          
          showModal( modalDialog(h2("¡Error!"), h4("No se puede acceder a la carpeta de salida"),
                                 helpText("El nombre de la carpeta contine caracteres inválidos"),
                                 helpText("o no se tienen los permisos necesarios"),
                                 footer=modalButton("Cerrar"), size="m"))
          
        }
        
      }
      
      enable("btn_graphPCA")
      
      
      remove_modal_spinner()
    })
    
    ##### ploteo
    
    observeEvent(input$btn_graphPCA ,{
      
      
      if( input$tipoGrafPCA == "Variables"){
        
        output$PCAplot<- renderPlot(execOnResize =T,
                                    isolate(  plot.PCA(res$PCA , title = "PCA variables" ,axes = c( as.numeric(gsub("Dim.","", input$grafPCA_X)) , as.numeric(gsub("Dim.","", input$grafPCA_Y))), choix = "var",  graph.type="classic") )
        )
        
      }else{
        if(input$clasesVarPCA == "Todos los datos"){
          
          output$PCAplot<- renderPlot(execOnResize =T,
                                      isolate(  plot.PCA(res$PCA, title = "PCA individuos" , axes = c( as.numeric(gsub("Dim.","", input$grafPCA_X)) , as.numeric(gsub("Dim.","", input$grafPCA_Y))), choix = "ind", label="none", graph.type="classic"))
          )
          
        }else{
          
          if(input$VarLevelsPCA == "Todas las clases"){
            
            tmp <- datosPCA$df[,input$clasesVarPCA]
            
            levels(tmp)<- PCAcols$cols
            tmp<-as.character(tmp)
            
            
            output$PCAplot<- renderPlot(execOnResize =T,{
              isolate(plot.PCA(res$PCA, title = "PCA individuos [Color]" , axes = c( as.numeric(gsub("Dim.","", input$grafPCA_X)) , as.numeric(gsub("Dim.","", input$grafPCA_Y))), choix = "ind", label ="none", habillage ="ind", col.hab= tmp,  graph.type="classic"))
              isolate( legend("topright", legend= levels(datosPCA$df[,input$clasesVarPCA]) , levels(datosPCA$df[,input$clasesVarPCA]), text.col=PCAcols$cols, bty="n") )
            }) 
            
          }else{
            
            tmp <- datosPCA$df[,input$clasesVarPCA]
            
            levels(tmp)<- PCAcols$cols
            tmp<-as.character(tmp)
            
            sel<- which(input$VarLevelsPCA == datosPCA$df[,input$clasesVarPCA])
            
            output$PCAplot<- renderPlot(execOnResize =T,{
              isolate(plot.PCA(res$PCA, title = paste0("PCA individuos [", input$VarLevelsPCA, "]") , axes = c( as.numeric(gsub("Dim.","", input$grafPCA_X)) , as.numeric(gsub("Dim.","", input$grafPCA_Y))), choix = "ind", label ="none", habillage ="ind", col.hab= tmp,  graph.type="classic", select=sel, unselect=1))
              isolate( legend("topright", legend= levels(datosPCA$df[,input$clasesVarPCA]) , levels(datosPCA$df[,input$clasesVarPCA]), text.col=PCAcols$cols, bty="n") )
            }) 
            
            
          }
          
        }
        
        
      }
      
      updateTabsetPanel(session, "PCAtabs",
                        selected = "Gráfica")
      
    })
    
    
    ###### controles numero de PCA's
    
    observeEvent(input$dimsPCA,{
      
      if(!input$dimsPCA ){
        #show("dimsPCAmanual")
        output$nPCAs<- renderUI(
          numericInput("dimsPCAmanual","¿Cuántas dimensiones calcular?", value = length(input$varsPCA), step=0.5)
        )
        
      }else{
        hide("dimsPCAmanual")
      }
    })
    
    ##### ruta salida resumen pca
    observeEvent(input$saveSummaryPCA,{
      
      if(input$saveSummaryPCA ){
        output$outDirSummaryPCA<- renderUI(
          textInput("outDirPCA", "Directorio de salida:", placeholder = "---" )
        )
        
      }else{
        hide("outDirPCA")
      }
    })
    
    observeEvent(input$dimsPCAmanual,{
      
      if( any(input$dimsPCAmanual > length(input$varsPCA), length(input$varsPCA) == 0, is.na(input$dimsPCAmanual), input$dimsPCAmanual < 2  )){
        reset("dimsPCAmanual")
      }
    })
    
    
    observeEvent(list(input$varsPCA,input$saveSummaryPCA, input$outDirPCA),{
      if(length(input$varsPCA) >= 2){
        
        if( all(input$saveSummaryPCA, input$outDirPCA!="") ){
          enable("btn_runPCA")
        }else if(!input$saveSummaryPCA){
          enable("btn_runPCA") 
        }else{
          disable("btn_runPCA")
        }
        
      }else{
        disable("btn_runPCA")
      }
    })
    
    
    
    
    
    
    observeEvent(input$tipoGrafPCA, {
      
      if(input$tipoGrafPCA == "Individuos"){
        enable("clasesVarPCA")
        if( input$clasesVarPCA == "Todos los datos" ){
          disable("VarLevelsPCA")  
        }else{
          enable("VarLevelsPCA")  
        }
        
      }else{
        disable("clasesVarPCA")
        disable("VarLevelsPCA")
      }
    })
    
    
    
    observeEvent(input$clasesVarPCA,{
      
      if(input$clasesVarPCA == "Todos los datos"){
        disable("VarLevelsPCA")
        output$levelsVarsPCAFact<- renderUI(
          disabled( selectInput("VarLevelsPCA", "Clase:", choices = c("---"), selected = "---", width = "100%")  )
        ) 
      }else{
        enable("VarLevelsPCA")
        
        output$levelsVarsPCAFact<- renderUI(
          selectInput("VarLevelsPCA", "Clase:", choices = c("Todas las clases", levels(datosPCA$df[,input$clasesVarPCA]) ), selected = "Todas las clases", width = "100%") 
        ) 
        
        PCAcols$cols <- randomColor(length( levels(datosPCA$df[,input$clasesVarPCA]) ), hue = c(" ", "random", "red", "orange", "yellow","green", "blue", "purple", "pink", "monochrome"),luminosity = c(" ","random", "light", "bright", "dark"))
        
      }
      
      
      
      
    })
    
    
    
    observeEvent(input$outDirGraphsPCA,{
      
      if(input$outDirGraphsPCA != ""){
        enable("btn_graphPCAsave")
      }else{
        disable("btn_graphPCAsave")
      }
      
    })
    
    
    
    observeEvent(input$btn_graphPCAsave,{
      
      if(dir.exists( input$outDirGraphsPCA )){
        
        wd.out<- input$outDirGraphsPCA
        
        if( input$tipoGrafPCA == "Variables"){
          
          png( paste0(wd.out, "/PCA_Var_dim",gsub("Dim.","", input$grafPCA_X),"-", gsub("Dim.","", input$grafPCA_Y),".png"), width = 480*3, height = 480*3, pointsize = 26 )
          plot.PCA(res$PCA, title = "PCA variables" , axes = c( as.numeric(gsub("Dim.","", input$grafPCA_X)) , as.numeric(gsub("Dim.","", input$grafPCA_Y))), choix = "var",  graph.type="classic")
          dev.off()
          
        }else{
          if(input$clasesVarPCA == "Todos los datos"){
            
            png( paste0(wd.out, "/PCA_Ind_dim",gsub("Dim.","", input$grafPCA_X),"-", gsub("Dim.","", input$grafPCA_Y),".png"), width = 480*3, height = 480*3, pointsize = 26 )
            plot.PCA(res$PCA, title = "PCA individuos" , axes = c( as.numeric(gsub("Dim.","", input$grafPCA_X)) , as.numeric(gsub("Dim.","", input$grafPCA_Y))), choix = "ind", label="none", graph.type="classic")
            dev.off()
            
          }else{
            
            if(input$VarLevelsPCA == "Todas las clases"){
              
              tmp <- datosPCA$df[,input$clasesVarPCA]
              
              levels(tmp)<- PCAcols$cols
              tmp<-as.character(tmp)
              
              png( paste0(wd.out, "/PCA_ColorInd_dim",gsub("Dim.","", input$grafPCA_X),"-", gsub("Dim.","", input$grafPCA_Y),".png"), width = 480*3, height = 480*3, pointsize = 26 )
              
              plot.PCA(res$PCA, title = "PCA individuos [Color]" , axes = c( as.numeric(gsub("Dim.","", input$grafPCA_X)) , as.numeric(gsub("Dim.","", input$grafPCA_Y))), choix = "ind", label ="none", habillage ="ind", col.hab= tmp,  graph.type="classic")
              isolate( legend("topright", legend= levels(datosPCA$df[,input$clasesVarPCA]) , levels(datosPCA$df[,input$clasesVarPCA]), text.col=PCAcols$cols, bty="n") )
              dev.off()
              
              
            }else{
              
              tmp <- datosPCA$df[,input$clasesVarPCA]
              
              levels(tmp)<- PCAcols$cols
              tmp<-as.character(tmp)
              
              sel<- which(input$VarLevelsPCA == datosPCA$df[,input$clasesVarPCA])
              
              png( paste0(wd.out, "/PCA_ColorInd_dim",gsub("Dim.","", input$grafPCA_X),"-", gsub("Dim.","", input$grafPCA_Y),"_",input$VarLevelsPCA,".png"), width = 480*3, height = 480*3, pointsize = 26 )
              
              plot.PCA(res$PCA, title = paste0("PCA individuos [", input$VarLevelsPCA, "]") , axes = c( as.numeric(gsub("Dim.","", input$grafPCA_X)) , as.numeric(gsub("Dim.","", input$grafPCA_Y))), choix = "ind", label ="none", habillage ="ind", col.hab= tmp,  graph.type="classic", select=sel, unselect=1)
              isolate( legend("topright", legend= levels(datosPCA$df[,input$clasesVarPCA]) , levels(datosPCA$df[,input$clasesVarPCA]), text.col=PCAcols$cols, bty="n") )
              
              dev.off()
              
            }
            
          }
          
          
        }
        
        
        
      }else{
        
        showModal( modalDialog(h2("¡Error!"), h4("No se puede acceder a la carpeta de salida"),
                               helpText("El nombre de la carpeta contine caracteres inválidos"),
                               helpText("o no se tienen los permisos necesarios"),
                               footer=modalButton("Cerrar"), size="m"))
      }
      
      
    })
    
    
    
    ######
    
    
    ####### server kmedias #####
    
    ##### inicializacion 
    datosKmedias<- reactiveValues(df="")
    colorKmedias<- reactiveValues(df="")
    datosResKmedias<-reactiveValues(df="")
    
    output$ejeXkmedias<- renderUI(
        disabled(selectInput("KmediasEjeX", NULL, choices = "---", selected = "---" ))
    )
    
    output$ejeYkmedias<- renderUI(
        disabled(selectInput("KmediasEjeY", NULL, choices = "---", selected = "---" )  )
    )
    
    
    output$Nclusters<- renderUI({
        disabled(selectInput("KmediasGrupos", NULL, choices = c("---" ), selected = "---" ))
    })
    
    
    
    
    
    ##### carga los datos
    observeEvent(input$dataKmedias,{
        
        
        if( !is.null( input$dataKmedias$datapath ) ){
            
            datosKmedias$df<- read.table(input$dataKmedias$datapath, header=T, sep=",", stringsAsFactors = T)
            datosKmedias$df<- na.omit(datosKmedias$df)
            
            colorKmedias$df<- data.frame(ID=1:dim(datosKmedias$df)[1])
            #print(str(datosKmedias$df))
            
            varsKmediastmp<-names(datosKmedias$df)[lapply(datosKmedias$df,class)!="factor"]
            FactvarsKmediastmp<-names(datosKmedias$df)[lapply(datosKmedias$df,class)=="factor"]
            
            output$VarsKmediasCheckbox<- renderUI(
                checkboxGroupInput("varsKmedias", NULL ,choices = varsKmediastmp, selected = varsKmediastmp) 
            ) 
            
            output$ejeXkmedias<- renderUI(
                selectInput("KmediasEjeX", NULL, choices = c("---",varsKmediastmp), selected = "---" )
            )
            
            output$ejeYkmedias<- renderUI(
                selectInput("KmediasEjeY", NULL, choices = c("---",varsKmediastmp), selected = "---" )  
            )
            
            
            output$Nclusters<- renderUI({
                disabled(selectInput("KmediasGrupos", NULL, choices = c("---" ), selected = "---" ))
            })
            
            
            enable("btn_runKmedias")
        }
        
    }) 
    
    
    #### controles
    
    observeEvent(input$saveKmedias,{
        if(input$saveKmedias){
            enable("outDirKmedias")
        }else{
            disable("outDirKmedias")
        }
    })
    
    
    observeEvent(list(input$saveKmedias, input$outDirKmedias),{
        
        
        if( all(input$saveKmedias, dir.exists(input$outDirKmedias))  ){
            enable("btn_runKmedias")
        }else if(!input$saveKmedias){
            enable("btn_runKmedias") 
        }else{
            disable("btn_runKmedias")
        }
        
        
    })
    
    
    observeEvent(input$MinGruposKmedias,{
        if( any( is.na(input$MinGruposKmedias), input$MinGruposKmedias<2) ){
            reset("MinGruposKmedias")
        }
    })
    
    observeEvent(input$MaxGruposKmedias,{
        if( any( is.na(input$MaxGruposKmedias), input$MaxGruposKmedias<2) ){
            reset("MaxGruposKmedias")
        }
    })
    
    
    observeEvent(list(input$outDirKmediasPlots, input$KmediasGrupos, input$KmediasEjeY, input$KmediasEjeX),{
        if( all( input$KmediasEjeX!="---", input$KmediasEjeY!="---", input$KmediasGrupos!="---", !is.null(input$KmediasEjeX),!is.null(input$KmediasEjeY), !is.null(input$KmediasGrupos),dir.exists(input$outDirKmediasPlots)  ) ){
            enable("btn_saveKmediasGraph")
        }else{
            disable("btn_saveKmediasGraph")
        }
    })
    
    
    ##### corre el kmedias
    
    
    observeEvent(input$btn_runKmedias,{
        
        show_modal_spinner()
        
        datosResKmedias$df<- as.data.frame(datosKmedias$df[,input$varsKmedias])
        names(datosResKmedias$df)<-input$varsKmedias
        
        if(dim(datosResKmedias$df)[2] == 0 ){
            
            showModal( modalDialog(h2("¡Error!"), h4("Seleccione al menos una variable"),
                                   footer=modalButton("Cerrar"), size="m"))
            
            
        }else{
            
            for(i in input$MinGruposKmedias:input$MaxGruposKmedias){
                
                grupos<- kmeans(datosResKmedias$df, i,iter.max=100)
                datosResKmedias$df[paste0("Grupos", i)]<- grupos$cluster
                
                
                colorKmedias$df[paste0("Grupos", i)] <- as.character(factor(datosResKmedias$df[,paste0("Grupos", i)], unique(datosResKmedias$df[,paste0("Grupos", i)]),
                                                                            randomColor(i)
                ))
                
                
            }
            
            
            output$Nclusters<- renderUI({
                
                selectInput("KmediasGrupos", NULL, choices = c("---", names(datosResKmedias$df)[grepl("Grupos", names(datosResKmedias$df))] ), selected = "---" )
                
            })
            
            
            if(input$saveKmedias){
                
                log.tmp <- file.path( paste0(input$outDirKmedias,"/", "Kmedias_Resumen.log" ) )
                lf <- log_open(log.tmp, show_notes=F, logdir = F)
                
                log_print("------- Archivo creado ------", console = F)
                
                log_print(paste0(input$outDirKmedias,"/", "Kmedias_Resumen.log" ), console = F )
                log_print(paste0(input$outDirKmedias,"/", "Kmedias_Resultado.csv" ), console = F )
                
                
                log_print("------- Variables dse entrada ------", console = F)
                log_print(input$varsKmedias, console = F)
                
                log_print("------- Resumen ------", console = F)
                
                
                #log_print( print(str(datosResKmedias$df)), console=F)
                
                kmeansTmp<-datosResKmedias$df
                
                log_print(print(ls.str( pattern="kmeansTmp"  )), console=F)
                
                log_close()
                
                
                write.table(datosResKmedias$df, paste0(input$outDirKmedias,"/", "Kmedias_Resultado.csv" ), sep=",", col.names=T,row.names=F)
                
                
            }
            
            
            
        }
        
        remove_modal_spinner()
        
    })
    
    
    ###### grafica clusters
    
    observeEvent(list( input$KmediasEjeX, input$KmediasEjeY, input$KmediasGrupos ),{
        
        if( all( input$KmediasEjeX!="---", input$KmediasEjeY!="---", input$KmediasGrupos!="---", !is.null(input$KmediasEjeX),!is.null(input$KmediasEjeY), !is.null(input$KmediasGrupos)  ) ){
            
            enable("btn_plotKmediasGraph")
            
        }else{
            
            disable("btn_plotKmediasGraph")
            
        }
        
        
    })
    
    
    ##### boton graficar 
    
    
    observeEvent(input$btn_plotKmediasGraph,{
        
        if( all( input$KmediasEjeX!="---", input$KmediasEjeY!="---", input$KmediasGrupos!="---", !is.null(input$KmediasEjeX),!is.null(input$KmediasEjeY), !is.null(input$KmediasGrupos)  ) ){
            
            t<-tapply(colorKmedias$df[,input$KmediasGrupos], datosResKmedias$df[,input$KmediasGrupos], unique)
            
            output$KmediasPlot<- renderPlot({
                
                isolate( plot(datosKmedias$df[,input$KmediasEjeX], datosKmedias$df[,input$KmediasEjeY], pch=19, cex=0.75, col=colorKmedias$df[,input$KmediasGrupos],
                              main=paste0("Análisis de agrupación, K medias"), xlab=input$KmediasEjeX, ylab=input$KmediasEjeY) )
                isolate(legend("topright", names(t), names(t), text.col=as.vector(t), bty="n", cex=1.5) )
                
            })
            
        }else{
            output$KmediasPlot<- renderPlot({
                plot.new()
            })
            
        }
        
        
    })
    
    ##### guarda grafica
    
    
    observeEvent(input$btn_saveKmediasGraph,{
        
        wd.out<- input$outDirKmediasPlots
        t<-tapply(colorKmedias$df[,input$KmediasGrupos], datosResKmedias$df[,input$KmediasGrupos], unique)
        
        
        png( paste0(wd.out, "/Kmedias_",input$KmediasGrupos,".png"), width = 480*3, height = 480*3, pointsize = 26 )
        
        plot(datosKmedias$df[,input$KmediasEjeX], datosKmedias$df[,input$KmediasEjeY], pch=19, cex=0.75, col=colorKmedias$df[,input$KmediasGrupos],
             main=paste0("Análisis de agrupación, K medias"), xlab=input$KmediasEjeX, ylab=input$KmediasEjeY) 
        legend("topright", names(t), names(t), text.col=as.vector(t), bty="n", cex=1.5) 
        
        dev.off()
        
        
    })
    
    
    
    #####
    
    
    
    
    
}### fin server

shinyApp(ui = ui, server = server)




