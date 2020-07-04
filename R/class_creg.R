#' Initialization of the class "creg"
#'
#' @description Using  the setClass() function to initialize the class of creg
#' objects, for generic objects
#' @export
setClass("creg")





#' @title Print function for class creg
#'
#' @description This function implements the generic print functions for the
#' S3 class creg.
#'
#' @param x creg object
#' @param ... optionally more fitted model objects.
#'
#' @details The print() function returns the Log-likelihood of the
#'  copula regression
#' @export
print.creg <- function(x,...){if (class(x)!="creg"){
  stop("The argument has to be from the class creg")}else{
   print("Result Log-Likelihood");print(x$result$Likelihood)}}




#' @title Summary function for class creg
#'
#' @description This function implements the generic summary functions for the
#' S3 class creg
#'
#' @param x creg object
#' @param ... As an additional option, it is possible to add param=TRUE to
#' display the parameters of the distribution.
#'
#' @details Through the summary() function  basic details are returned.
#'  If wanted, additional information can be collected through including the
#'  paramter param=TRUE. Accordingly,  the summary function will also
#'  return the parameters of the distributions and the copula, both prior and
#'  after the transformation.
#' @export
summary.creg <- function(x,...){return (summarize(x,...))}







#' @title Plot function for class creg
#'
#' @description This function implements the generic plot functions for the
#' S3 class creg
#'
#' @param x creg object
#' @param ... optionally more fitted model objects, e.g. option="likelihood2D"
#' @details The next generic function is the plot() function, which uses ggplots to
#' visualize the results. Similar to other generic plot functions, it is
#' possible to recieve different plots, by including the additional parameter
#' option e.g. \code{plot(x,option="likelihood3D")}.#'
#' The following plots are available within the function:
#' "likelihood2D" and "likelihood3D" display the individual likelihood of each
#' observation in a 2D or 3D plot. Note that the value of the likelihood
#' corresponds with the result of the joint density.
#' In addition, it is possible to plot the results of the copula density by
#' setting option="copula3D". If option="dataview" is selected, three plots for the
#' dataset are plotted. The default selection for option is "likelihood2D".
#' @export
plot.creg <- function(x,...){return(plotting(x,...))}





#' @title Generic AIC function for class creg
#'
#' @description This function implements the generic AIC function for the S3 class
#' creg
#'
#' @param object A creg object
#' @param k Penalty term (default is 2)
#' @param ... optionally more fitted model objects.
#' @export
AIC.creg <- function(object,...,k=2){return(
  2 * ncol(object$Transformed)- k *object$result$Likelihood)}



#'  @title Generic BIC function for class creg
#'
#' @description This function implements the generic BIC function for the S3 class
#' creg
#' @param object A creg object
#' @param ... optionally more fitted model objects.
#'
#'
#' @export
BIC.creg <- function(object, ...){return(log(
  nrow(object$input$data)) * ncol(
    object$Transformed) - 2 *object$result$Likelihood) }





################################################################################

#                           Assisting Functions                                #

################################################################################
#' Assisting Summary function for class creg
#'
#' @description This function implements the generic summary functions for the
#' S3 class creg
#'
#' @param x creg object
#' @param parameter Logical, can be used to display the parameters of the
#' distribution.
#' @param ... optionally more fitted model objects.
#'
#' @details Through the summary() function  basic details are returned.
#'  If wanted, additional information can be collected through including the
#'  paramter param=TRUE. Accordingly,  the summary function will also
#'  return the parameters of the distributions and the copula, both prior and
#'  after the transformation.
#'  \code{summary(x,param=TRUE)}
summarize <- function(x,parameter,...) {
  if (class(x)!="creg"){stop("The argument has to be from the class creg")}else{
    if (missing(parameter)){parameter<-FALSE}
    if(parameter==FALSE){

      out <- list("Provided betas for the parameters:"=x$input$beta,
                                 "Number of resulting parameters:"=ncol(x$Non_transformed),
                                 "Chosen Copula:"=x$input$copula,
                                 "Result Log-Likelihood"=x$result$Likelihood);
    return(out)
    }else{
      if(parameter==TRUE){
        out <- list("Provided betas for the parameters:"=x$input$beta,
                    "Number of resulting parameters:"=ncol(x$Non_transformed),
                    "Chosen Copula:"=x$input$copula,
                    "Result Log-Likelihood"=x$result$Likelihood,
                    "Parameter before transformation"=x$Non_transformed,
                    "Parameter after transformation"=x$Transformed);
        return(out)
      }else{stop("Param is a logical parameter. Please input either TRUE or FALSE")}}};
}


#' @title Assisting Plot function for class creg
#'
#' @description This function implements the generic plot functions for the
#' S3 class creg
#'
#' @param x creg object
#' @param option Object to define the respective plot.
#' @param ... optionally more fitted model objects.
#' @details The next generic function is the plot() function, which uses ggplots to
#' visualize the results. Similar to other generic plot functions, it is
#' possible to recieve different plots, by including the additional parameter
#' option e.g. \code{plot(x,option="likelihood3D")}.#'
#' The following plots are available within the function:
#' "likelihood2D" and "likelihood3D" display the individual likelihood of each
#' observation in a 2D or 3D plot. Note that the value of the likelihood
#' corresponds with the result of the joint density.
#' In addition, it is possible to plot the results of the copula density by
#' setting option="copula3D". If option="dataview" is selected, three plots for the
#' dataset are plotted. The default selection for option is "likelihood2D".
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 scale_colour_gradient
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 geom_density
#' @importFrom plotly plot_ly
#' @importFrom plotly add_markers
#' @importFrom plotly layout
#' @importFrom gridExtra grid.arrange
#'
plotting  <- function(x,option, ...){
  if (class(x)!="creg"){stop("The argument has to be from the class creg")};
  if (missing(option)){option<- "likelihood2D"}
  if (option=="likelihood2D"){
    input <- as.data.frame(cbind(x$input$data,x$result$Joint_density));
    colnames(input) <- c("X","Y","density");
    P <- ggplot2::ggplot()+ggplot2::geom_point(
      ggplot2::aes(x=X,y=Y,color=density),alpha=0.4,data = input,
      show.legend = TRUE)+ggplot2::scale_colour_continuous(
        low = "blue", high = "red");return(P);
  }else{if (option=="likelihood3D"){X <- x$input$data[,1];
  Y <-  x$input$data[,2];
  density <- x$result$Cdensity;
  P <- plotly::plot_ly( x =~X, y = ~Y, z = ~density,size=0.5,
                        marker = list(color = ~density,
                                      colorscale = "Hot",
                                      showscale = TRUE));
  P <- plotly::add_trace(P, type="scatter3d",mode="markers");
  P <-  plotly::add_markers(P);
  P <-  plotly::layout(P,scene = list(xaxis = list(title = 'X'),
                                      yaxis = list(title = 'Y'),
                                      zaxis = list(title = 'Likelihood')));
  return(P)}else{if(option=="copula3D"){
    X <- x$input$data[,1];
    Y <-  x$input$data[,2];
    density <- x$result$Cdensity;
    P <- plotly::plot_ly( x =~X, y = ~Y, z = ~density,size=2.5,
                          marker = list(color = ~density,
                                        colorscale = c('#00CCFF', '#0000FF'),
                                        showscale = TRUE));
    P <- plotly::add_trace(P, type="scatter3d",mode="markers");
    P <-  plotly::add_markers(P);
    P <-  plotly::layout(P,scene = list(xaxis = list(title = 'X'),
                                        yaxis = list(title = 'Y'),
                                        zaxis = list(title = 'Copula Density')));
    return(P);
  }else{if (option=="dataview"){
    i1 <- as.data.frame(x$input$data);
    colnames(i1) <- c("X","Y");
    P1 <- ggplot2::ggplot()+ggplot2::geom_point(ggplot2::aes(x=X,y=Y),
                                                alpha=0.4,data =i1,color="#0000FF")+
      ggplot2::ggtitle("Scatterplot of Data");
    P2 <-ggplot2::ggplot(ggplot2::aes(x=X),data=i1) +
      ggplot2::geom_density(color="#0000FF")+
      ggplot2::ggtitle("Estimated density of first data input");
    P3 <-ggplot2::ggplot(ggplot2::aes(x=Y),data=i1) +
      ggplot2::geom_density(color="#0000FF")+
      ggplot2::ggtitle("Estimated density of second data input");
    return(gridExtra::grid.arrange(P1, P2,P3, nrow = 3,
                                   layout_matrix=matrix(c(1,1,2,1,1,3),3,2)));
  }else{stop("This option is not supported.")}}}};
}
