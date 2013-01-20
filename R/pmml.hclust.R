# PMML: Predictive Modelling Markup Language
#
# Part of the Rattle package for Data Mining
#
# Time-stamp: <2009-10-25 20:51:42 Graham Williams>
#
# Copyright (c) 2009 Togaware Pty Ltd
#
# This files is part of the Rattle suite for Data Mining in R.
#
# Rattle is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# Rattle is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Rattle. If not, see <http://www.gnu.org/licenses/>.

########################################################################

pmml.hclust <- function(model,
                        model.name="HClust_Model",
                        app.name="Rattle/PMML",
                        description="Hierarchical cluster model",
                        copyright=NULL,
                        transforms=NULL,
                        dataset=NULL,
                        centers,
                        ...)
{
  require(XML, quietly=TRUE)
  
  if (! inherits(model, "hclust")) stop("Not a legitimate hclust object")

  # Collect the required information.

  field <- NULL
  field$name <-  colnames(centers)
  number.of.fields <- length(field$name)
  field$class <- rep("numeric", number.of.fields) # All fields are numeric
  names(field$class) <- field$name

  field2 <- NULL
  field2$name[1] <- "ZementisClusterIDPlaceHolder"
  field2$class[1] <- "ID"
  names(field2$class)[1] <- "ZementisClusterIDPlaceHolder"
  for(i in 1:number.of.fields)
  {
   field2$name[i+1] <- field$name[i]
   field2$class[i+1] <- field$class[i]
   names(field2$class)[i+1] <- names(field$class[i])
  }

  orig.fields <- field$name

  # 090822 Mark any categoric transforms as inactive since they won't
  # have been used in the clustering (at least not until we automate
  # the conversion to indicator variables.

#  for (i in which(sapply(transforms, function(x) x$type) %in%
#                  .TRANSFORMS.TO.CATEGORIC))
#    transforms[[i]]$status <- "inactive"
  
  if (.supportTransformExport(transforms))
  {
    field <- .unifyTransforms(field, transforms, keep.first=FALSE)
    transforms <- .activateDependTransforms(transforms)
  }

  number.of.clusters <- nrow(centers)
  cluster.names <- 1:number.of.clusters

  # PMML

  pmml <- .pmmlRootNode("4.1")

  # PMML -> Header

  pmml <- append.XMLNode(pmml, .pmmlHeader(description, copyright, app.name))

  # PMML -> DataDictionary

  pmml <- append.XMLNode(pmml, .pmmlDataDictionary(field2,transformed=transforms))

  # PMML -> ClusteringModel

  cl.model <- xmlNode("ClusteringModel",
                      attrs=c(modelName=model.name,
                        functionName="clustering", # Required
                        algorithmName="HClust",
                        modelClass="centerBased", # Required
                        numberOfClusters=number.of.clusters)) # Required

  # PMML -> ClusteringModel -> MiningSchema

  cl.model <- append.XMLNode(cl.model, .pmmlMiningSchema(field2,transformed=transforms))

  # Outputs
  output <- xmlNode("Output")
  out <- xmlNode("OutputField",attrs=c(name="predictedValue", feature="predictedValue"))
  output <- append.XMLNode(output, out)
  cl.model <- append.XMLNode(cl.model, output)

  # PMML -> ClusteringModel -> LocalTransformations -> DerivedField -> NormContiuous

  if (.supportTransformExport(transforms))
    cl.model <- append.XMLNode(cl.model, .gen.transforms(transforms))

  # test of Zementis xform functions
  if(!is.null(transforms))
  {
    the.model <- append.XMLNode(the.model, pmmlLocalTransformations(field2, transforms))
  }
  
  # PMML -> ClusteringModel -> ComparisonMeasure
  
  cl.model <- append.XMLNode(cl.model,
                             append.XMLNode(xmlNode("ComparisonMeasure",
                                                    attrs=c(kind="distance")),
                                            xmlNode("squaredEuclidean")))

  # PMML -> ClusteringField

  for (i in orig.fields)
  {
    cl.model <- append.xmlNode(cl.model,
                               xmlNode("ClusteringField",
                                       attrs=c(field=i)))
  }
  
  # PMML -> ClusteringModel -> Cluster -> Array
  
  clusters <- list()
  for (i in 1:number.of.clusters)
  {
    cl.model <- append.XMLNode(cl.model,
                               xmlNode("Cluster",
                                       attrs=c(name=cluster.names[i],
                                         size=model$size[i]),
                                       xmlNode("Array",
                                               attrs=c(n=number.of.fields,
                                                 type="real"),
                                               paste(centers[i,],
                                                     collapse=" "))))
  }
  pmml <- append.XMLNode(pmml, cl.model)

  return(pmml)
}
