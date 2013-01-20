# PMML: Predictive Modelling Markup Language
#
# Part of the Rattle package for Data Mining
#
# Time-stamp: <2012-02-19 17:54:22 Graham Williams>
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

pmml.kmeans <- function(model,
                        model.name="KMeans_Model",
                        app.name="Rattle/PMML",
                        description="KMeans cluster model",
                        copyright=NULL,
                        transforms=NULL,
                        dataset=NULL,
                        algorithm.name="KMeans: Hartigan and Wong",
                        ...)
{
  if (! inherits(model, "kmeans")) stop("Not a legitimate kmeans object")
  require(XML, quietly=TRUE)
  
  # Collect the required information.

  field <- NULL
  field$name <-  colnames(model$centers)
  number.of.fields <- length(field$name)
  field$class <- rep("numeric", number.of.fields) # All fields are numeric
  names(field$class) <- field$name
  orig.fields <- field$name

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

  # 090808 Mark any categoric transforms as inactive since they won't
  # have been used in the clustering (at least not until we automate
  # the conversion to indicator variables.

#  for (i in which(sapply(transforms, function(x) x$type) %in%
#                  .TRANSFORMS.TO.CATEGORIC))
#  {
#    transforms[[i]]$status <- "inactive"
#  }
  
  if (.supportTransformExport(transforms))
  {
    field <- .unifyTransforms(field, transforms, keep.first=FALSE)
    transforms <- .activateDependTransforms(transforms)
  }
  
  number.of.clusters <- length(model$size)
  cluster.names <- rownames(model$centers)

  # PMML

  pmml <- .pmmlRootNode("4.1")

  # PMML -> Header

  pmml <- append.XMLNode(pmml, .pmmlHeader(description, copyright, app.name))

  # PMML -> DataDictionary

  pmml <- append.XMLNode(pmml, .pmmlDataDictionary(field2,transformed=transforms))

  # PMML -> ClusteringModel

  the.model <- xmlNode("ClusteringModel",
                      attrs=c(modelName=model.name,
                        functionName="clustering", # Required
                        algorithmName=algorithm.name,
                        modelClass="centerBased", # Required
                        numberOfClusters=number.of.clusters)) # Required

  # PMML -> ClusteringModel -> MiningSchema

  the.model <- append.XMLNode(the.model, .pmmlMiningSchema(field2,transformed=transforms))

  # Outputs
  output <- xmlNode("Output")
  out <- xmlNode("OutputField",attrs=c(name="predictedValue", feature="predictedValue"))
  output <- append.XMLNode(output, out)
  the.model <- append.XMLNode(the.model, output)

  # PMML -> ClusteringModel -> LocalTransformations -> DerivedField -> NormContiuous

  if (.supportTransformExport(transforms))
    the.model <- append.XMLNode(the.model, .gen.transforms(transforms))

  # test of Zementis xform functions
  if(!is.null(transforms))
  {
    the.model <- append.XMLNode(the.model, pmmlLocalTransformations(field2, transforms))
  }
 
  # PMML -> ClusteringModel -> ComparisonMeasure
  
  the.model <- append.XMLNode(the.model,
                             append.XMLNode(xmlNode("ComparisonMeasure",
                                                    attrs=c(kind="distance")),
                                            xmlNode("squaredEuclidean")))

  # PMML -> ClusteringField

  for (i in orig.fields)
  {
    the.model <- append.xmlNode(the.model,
                               xmlNode("ClusteringField",
                                       attrs=c(field=i,
                                         compareFunction="absDiff")))
  }
  
  # PMML -> ClusteringModel -> Cluster -> Array
  
  clusters <- list()
  for (i in 1:number.of.clusters)
  {
    the.model <- append.XMLNode(the.model,
                               xmlNode("Cluster",
                                       attrs=c(name=cluster.names[i],
                                         size=model$size[i]),
                                       xmlNode("Array",
                                               attrs=c(n=number.of.fields,
                                                 type="real"),
                                               paste(model$centers[i,],
                                                     collapse=" "))))
  }
  pmml <- append.XMLNode(pmml, the.model)

  return(pmml)
}

