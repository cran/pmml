# PMML: Predictive Model Markup Language
#
# Copyright (c) 2009-2016, Zementis, Inc.
# Copyright (c) 2016-2021, Software AG, Darmstadt, Germany and/or Software AG
# USA Inc., Reston, VA, USA, and/or its subsidiaries and/or its affiliates
# and/or their licensors.
#
# This file is part of the PMML package for R.
#
# The PMML package is free software: you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation, either version 3 of
# the License, or (at your option) any later version.
#
# The PMML package is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. Please see the
# GNU General Public License for details (http://www.gnu.org/licenses/).
# #############################################################################

#' Add a function transformation to a xform_wrap object.
#'
#' @param wrap_object Output of xform_wrap or another transformation function.
#' @param orig_field_name String specifying name(s) of the original data field(s) being used in the transformation.
#' @param new_field_name Name of the new field created by the transformation.
#' @param new_field_data_type R data type of the new field created by the transformation ("numeric" or "factor").
#' @param expression String expression specifying the transformation.
#' @param map_missing_to Value to be given to the transformed variable if the value of any input variable is missing.
#'
#' @details
#'
#' Calculate the expression provided
#' in \code{expression} for every row in the \code{wrap_object$data}
#' data frame. The \code{expression} argument must represent
#' a valid R expression, and any functions used in
#' \code{expression} must be defined in the current
#' environment.
#'
#' The name of the new field is optional (a default name is provided), but an error
#' will be thrown if attempting to create a field with a name that already exists in
#' the xform_wrap object.
#'
#' When \code{new_field_data_type = "numeric"}, the \code{DerivedField} attributes
#' in PMML will be \code{dataType = "double"} and \code{optype = "continuous"}.
#' When \code{new_field_data_type = "factor"}, these attributes will be
#' \code{dataType = "string"} and \code{optype = "categorical"}.
#'
#'
#' @return R object containing the raw data, the transformed data and data statistics.
#' The \code{data} data frame will contain a new \code{new_field_name} column, and
#' \code{field_data} will contain a new \code{new_field_name} row.
#'
#' @seealso \code{\link{xform_wrap}}
#'
#' @examples
#' # Load the standard iris dataset:
#' data(iris)
#'
#' # Wrap the data:
#' iris_box <- xform_wrap(iris)
#'
#' # Perform a transform on the Sepal.Length field:
#' # the value is squared and then divided by 100
#' iris_box <- xform_function(iris_box,
#'   orig_field_name = "Sepal.Length",
#'   new_field_name = "Sepal.Length.Transformed",
#'   expression = "(Sepal.Length^2)/100"
#' )
#'
#' # Combine two fields to create another new feature:
#' iris_box <- xform_function(iris_box,
#'   orig_field_name = "Sepal.Width, Petal.Width",
#'   new_field_name = "Width.Sum",
#'   expression = "Sepal.Width + Sepal.Length"
#' )
#'
#' # Create linear model using the derived features:
#' fit <- lm(Petal.Length ~
#' Sepal.Length.Transformed + Width.Sum, data = iris_box$data)
#'
#' # Create pmml from the fit:
#' fit_pmml <- pmml(fit, transform = iris_box)
#' @export
xform_function <- function(wrap_object, orig_field_name, new_field_name = "newField",
                           new_field_data_type = "numeric", expression, map_missing_to = NA) {
  if (!(new_field_data_type %in% c("numeric", "factor"))) {
    stop('new_field_data_type must be "numeric" or "factor".')
  }

  wrap_object$data$new_field_name <- NA


  parsed_text <- parse(text = expression)

  # Calculate the expression for each row in the new data column. For factors,
  # convert to character to avoid assigning integers to new_field_name. This
  # check is only necessary for when eval() is applied to a factor input.
  if (new_field_data_type == "numeric") {
    for (n in 1:length(wrap_object$data$new_field_name)) {
      boxrow <- wrap_object$data[n, ]
      wrap_object$data$new_field_name[n] <- eval(parsed_text, boxrow)
    }
  } else { # new_field_data_type == "factor"
    for (n in 1:length(wrap_object$data$new_field_name)) {
      boxrow <- wrap_object$data[n, ]
      wrap_object$data$new_field_name[n] <- toString(eval(parsed_text, boxrow))
    }
  }


  # for (n in 1:length(wrap_object$data$new_field_name)) {
  #   boxrow <- wrap_object$data[n, ]
  #   wrap_object$data$new_field_name[n] <- eval(parsed_text, boxrow)
  # }

  # Change class of new column to match new_field_data_type
  if (!(class(wrap_object$data$new_field_name) == new_field_data_type)) {
    if (new_field_data_type == "numeric") {
      wrap_object$data$new_field_name <- as.numeric(wrap_object$data$new_field_name)
    } else { # else convert to factor
      wrap_object$data$new_field_name <- as.factor(wrap_object$data$new_field_name)
    }
  }

  names(wrap_object$data)[names(wrap_object$data) == "new_field_name"] <- new_field_name

  # New column for formula; only create if doesn't already exist.
  # This is unnecessary if xform_function is already added by xform_wrap().
  if (!("xform_function" %in% colnames(wrap_object$field_data))) {
    wrap_object$field_data$xform_function <- "NA"
  }

  # Make new row with "NA" entries.
  temprow <- rep("NA", length(wrap_object$field_data))
  names(temprow) <- names(wrap_object$field_data)

  levels(wrap_object$field_data$type)[2] <- "derived" # must create factor level first

  temprow["type"] <- "derived"
  temprow["dataType"] <- new_field_data_type # this could be string
  temprow["xform_function"] <- expression
  # If orig_field_name contains multiple fields, these will be combined into one string.
  temprow["orig_field_name"] <- paste(orig_field_name, collapse = ",")
  if (!is.na(map_missing_to)) {
    temprow["missingValue"] <- map_missing_to
  }

  #
  # newrow <- data.frame(temprow, stringsAsFactors = TRUE)
  # colnames(newrow) <- colnames(wrap_object$field_data)

  # if temprow's dataType is not a factor level in wrap_object$field_data$dataType, add it
  if (!(unname(temprow["dataType"]) %in% levels(wrap_object$field_data$dataType))) {
    num_dataType_levels <- length(levels(wrap_object$field_data$dataType))
    levels(wrap_object$field_data$dataType)[num_dataType_levels + 1] <- unname(temprow["dataType"])
  }
  wrap_object$field_data <- rbind(wrap_object$field_data, temprow)

  # Add data to new row
  row.names(wrap_object$field_data)[nrow(wrap_object$field_data)] <- new_field_name



  # # Make new row with "NA" entries.
  # temprow <- matrix(c(rep.int("NA", length(wrap_object$field_data))), nrow = 1, ncol = length(wrap_object$field_data))
  # newrow <- data.frame(temprow, stringsAsFactors = TRUE)
  # colnames(newrow) <- colnames(wrap_object$field_data)
  # wrap_object$field_data <- rbind(wrap_object$field_data, newrow)
  #
  # # Add data to new row.
  # row.names(wrap_object$field_data)[nrow(wrap_object$field_data)] <- new_field_name
  #
  # levels(wrap_object$field_data$type)[2] <- "derived" # must create factor level first
  # wrap_object$field_data[new_field_name, "type"] <- "derived"
  # wrap_object$field_data[new_field_name, "dataType"] <- new_field_data_type # this could be string
  #
  #
  # wrap_object$field_data[new_field_name, "xform_function"] <- expression
  #
  # # If orig_field_name contains multiple fields, these will be combined into one string.
  # wrap_object$field_data[new_field_name, "orig_field_name"] <- paste(orig_field_name, collapse = ",")
  #
  # if (!is.na(map_missing_to)) {
  #   wrap_object$field_data[new_field_name, "missingValue"] <- map_missing_to
  # }


  return(wrap_object)
}
