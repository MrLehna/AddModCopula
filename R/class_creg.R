#' Initialization of the class "creg"
#'
#' @description Using  the setClass() function to initialize the class of creg
#' objects, for generic objects
#' @export
setClass("creg")







#' @title Summary function for class creg
#'
#' @description This function implements the generic summary functions for the
#' S3 class creg
#'
#' @param object creg object
#' @param ... Optional parameters e.g. param=TRUE
#'
#' @details Through the summary() function  basic details are returned.
#'  If wanted, additional information can be collected through including the
#'  parameter param=TRUE. Accordingly,  the summary function will also
#'  return the parameters of the distributions and the copula, both prior and
#'  after the transformation.
#'  \code{summary(object,param=TRUE)}
#'
#' @export
summary.creg <- function(object,...){return(summarize(x=object,...))}







#' @title Plot function for class creg
#'
#' @description This function implements the generic plot functions for the
#' S3 class creg
#'
#' @param x creg object
#' @param ... optional parameters e.g. option. See details for more information.
#'
#' @details The next generic function is the plot() function, which uses ggplots to
#' visualize the results. Similar to other generic plot functions, it is
#' possible to receive different plots, by including the additional parameter
#' option e.g. \code{plot(x,option="likelihood3D")}.#'
#' The following plots are available within the function:
#' "likelihood2D" and "likelihood3D" display the individual likelihood of each
#' observation in a 2D or 3D plot. Note that the value of the likelihood
#' corresponds with the result of the joint density.
#' In addition, it is possible to plot the results of the copula density by
#' setting option="copula3D". If option="dataview" is selected, three plots for the
#' data set are plotted. The default selection for option is "likelihood2D".
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 scale_colour_gradient
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 geom_density
#' @importFrom plotly plot_ly
#' @importFrom plotly add_markers
#' @importFrom plotly layout
#' @importFrom gridExtra grid.arrange
#' @method plot creg
#' @export
plot.creg <- function(x,...){return(plotting(x,...))}





#' @title Generic AIC function for class creg
#'
#' @description This function implements the generic AIC and BIC
#' for the S3 class creg
#'
#' @param object A creg object
#' @param k Penalty terms
#' @param ... Additional parameters e.g. BIC=TRUE
#'
#' @details It is possible to receive both the AIC and the BIC from the creg
#' object, by using the AIC function. An additional parameter (with
#' BIC=TRUE) can be passed into the function to receive the BIC value.
#'
#' @export
AIC.creg <- function(object,...,k=2){infcrit(object,...,k)}






#' @title Print function for class creg
#'
#' @description This function implements the generic print functions for the
#' S3 class creg.
#'
#' @param x creg object
#' @param ... Optional parameters
#' @details The print() function returns the Log-likelihood of the
#'  copula regression
#' @method print creg
#' @export
print.creg <- function(x,...){if (class(x)!="creg"){
  stop("The argument has to be from the class creg")}else{
    print("Result Log-Likelihood"); print(x$result$Likelihood)} }





################################################################################
#                   Helper Functions for Plot and Summary                      #
################################################################################




#' @title Helper Summary function for class creg
#'
#' @description This function implements the generic summary functions for the
#' S3 class creg
#'
#' @param x creg object
#' @param param Logical, can be used to display the parameters of the
#' distribution.
#' @param ... Additional Parameters optional
#'
#'
#' @details Through the summary() function  basic details are returned.
#'  If wanted, additional information can be collected through including the
#'  paramter param=TRUE. Accordingly,  the summary function will also
#'  return the parameters of the distributions and the copula, both prior and
#'  after the transformation.
#'  \code{summary(x,param=TRUE)}
summarize <- function(x,param,...){
  if (class(x)!="creg"){stop("The argument has to be from the class creg")}
  if (missing(param)){param <- FALSE}

  # print("Results of the creg calculation")
  if(param==FALSE){
    out <- list("Provided betas for the parameters:"=x$input$beta,
                "Number of distributional parameters:"=ncol(x$Non_transformed),
                "Chosen Copula:"=x$input$copula,
                "Result Log-Likelihood"=x$result$Likelihood)
  }else{
    if(param==TRUE){
      out <- list("Provided betas for the parameters:"=x$input$beta,
                  "Number of resulting parameters:"=ncol(x$Non_transformed),
                  "Chosen Copula:"=x$input$copula,
                  "Result Log-Likelihood"=x$result$Likelihood,
                  "Parameter before transformation"=x$Non_transformed,
                  "Parameter after transformation"=x$Transformed)
    }else{stop("Param is a logical parameter. Please input either TRUE or FALSE")}}
  return(out)
}


#' @title Helper function Plot function for class creg
#'
#' @description This function implements the generic plot functions for the
#' S3 class creg
#'
#' @param x creg object
#' @param option Object to define the respective plot.
#' @param ... optional parameters
#'
#' @details The next generic function is the plot() function, which uses ggplots to
#' visualize the results. Similar to other generic plot functions, it is
#' possible to receive different plots, by including the additional parameter
#' option e.g. \code{plot(x,option="likelihood3D")}.#'
#' The following plots are available within the function:
#' "likelihood2D" and "likelihood3D" display the individual likelihood of each
#' observation in a 2D or 3D plot. Note that the value of the likelihood
#' corresponds with the result of the joint density.
#' In addition, it is possible to plot the results of the copula density by
#' setting option="copula3D". If option="dataview" is selected, three plots for the
#' data set are plotted. The default selection for option is "likelihood2D".
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 scale_colour_gradient
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 geom_density
#' @importFrom plotly plot_ly
#' @importFrom plotly add_markers
#' @importFrom plotly layout
#' @importFrom gridExtra grid.arrange
plotting <- function(x,option,...){
  if (class(x)!="creg"){stop("The argument has to be from the class creg")}
  if (missing(option)){option <-"likelihood2D"}
  if (option=="likelihood2D"){
    input <- as.data.frame(cbind(x$input$data,x$result$Joint_density))
    colnames(input) <- c("X","Y","density")

    P <- ggplot2::ggplot()+ggplot2::geom_point(
      ggplot2::aes(x=X,y=Y,color=density),alpha=0.4,data = input,
      show.legend = TRUE)+
      ggplot2::scale_colour_continuous(low = "blue", high = "red")
    return(P)
  }else{
    if (option=="likelihood3D"){
      # input <- as.data.frame(cbind(x$input$data,x$result$Joint_density))
      # colnames(input) <- c("X","Y","density")
      # # For CHECK function
      # utils::globalVariables(c("X","Y" ,"density" ,"scatter3d"))

      X <- x$input$data[,1]
      Y <-  x$input$data[,2]
      density <- x$result$Cdensity

      P <- plotly::plot_ly( x =~X, y = ~Y, z = ~density,size=0.5,
                            marker = list(color = ~density,
                                          colorscale = "Hot",
                                          showscale = TRUE))

      P <- plotly::add_trace(P, type="scatter3d",mode="markers")
      P <-  plotly::add_markers(P)
      P <-  plotly::layout(P,scene = list(xaxis = list(title = 'X'),
                                          yaxis = list(title = 'Y'),
                                          zaxis = list(title = 'Likelihood')))
      return(P)

    }else{
      if(option=="copula3D"){
        # input <- as.data.frame(cbind(x$input$data,x$result$Cdensity))
        # colnames(input) <- c("X","Y","density")
        X <- x$input$data[,1]
        Y <-  x$input$data[,2]
        density <- x$result$Cdensity
        # For CHECK function
        # utils::globalVariables(c("X","Y" ,"density" ,"scatter3d"))

        P <- plotly::plot_ly( x =~X, y = ~Y, z = ~density,size=2.5,
                              marker = list(color = ~density,
                                            colorscale = c('#00CCFF', '#0000FF'),
                                            showscale = TRUE))
        P <- plotly::add_trace(P, type="scatter3d",mode="markers")
        P <-  plotly::add_markers(P)
        P <-  plotly::layout(P,scene = list(xaxis = list(title = 'X'),
                                            yaxis = list(title = 'Y'),
                                            zaxis = list(title = 'Copula Density')))
        return(P)

      }else{
        if (option=="dataview"){
          i1 <- as.data.frame(x$input$data)
          colnames(i1) <- c("X","Y")
          P1 <- ggplot2::ggplot()+ggplot2::geom_point(ggplot2::aes(x=X,y=Y),
                                                      alpha=0.4,data =i1,color="#0000FF")+
            ggplot2::ggtitle("Scatterplot of Data")
          P2 <-ggplot2::ggplot(ggplot2::aes(x=X),data=i1) +
            ggplot2::geom_density(color="#0000FF")+
            ggplot2::ggtitle("Estimated density of first data input")

          P3 <-ggplot2::ggplot(ggplot2::aes(x=Y),data=i1) +
            ggplot2::geom_density(color="#0000FF")+
            ggplot2::ggtitle("Estimated density of second data input")
          return(gridExtra::grid.arrange(P1, P2,P3, nrow = 3,
                                         layout_matrix=matrix(c(1,1,2,1,1,3),3,2)))

          # plot(P)


        }else{stop("This option is not supported.")}

      }
    }
  }
}

#' @title Helper Function of AIC and BIC
#'
#' @description This function implements the generic AIC and BIC
#' for the S3 class creg
#'
#' @param object A creg object
#' @param k Penalty terms
#' @param BIC Optional Parameters if BIC is required
#' @param ... Additional parameters
#'
#' @details It is possible to receive both the AIC and the BIC from the creg
#' object, by using the AIC function. An additional parameter (with
#' BIC=TRUE) can be passed into the function to receive the BIC value.

infcrit <- function(object,BIC,k,...){if (class(object)!="creg"){
  stop("The argument has to be from the class creg")}else{
    if (missing(BIC)){BIC <- FALSE}
    if (missing(k)){k <- 2}
    if (BIC==TRUE){
          return(log(nrow(object$input$data))*
                   ncol(object$Transformed)-2 *object$result$Likelihood)
    }else{return(k * ncol(object$Transformed)-2 *object$result$Likelihood)}}}
