# PMML: Predictive Modelling Markup Language
#
# Part of the Rattle package for Data Mining
#
# Add Transformations information to preexisting PMML model
#
# Time-stamp: <2013-08-10 19:48:25 Tridivesh Jena>
#
#
# This software is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# For a copy of the GNU General Public License, please see 
# <http://www.gnu.org/licenses/>.
#######################################################################

fileToXMLNode <- function(file) {

  filexml <- xmlTreeParse(file, useInternalNodes=FALSE)$doc$children[[1]]

  return(filexml) 
}
