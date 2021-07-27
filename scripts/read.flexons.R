
# 
# Read flexon data in a file such as: flexonData.7NX6.clean.
#
# Note!  There is a data item, 'fileIdx' below.  This is an index
# into the flexWork/atom.* and flexWork/align.* files, that is used 
# when there are multiple files for the same PDB code.  I have not
# used that feature here!  If there are more than two structures in
# the same PDB file, there may be flexon data for both of them in
# the input file, but only one will be read in.
#

read.flexons <- function(fname) {

	fdata <- scan(fname, what = character(), sep = "\n")
	listPDB <- list()
	pdbcode0 <- ""
	i <- 1
	flexList <- list()

	while (i <= length(fdata)) {
		line <- fdata[i]
		#print(line)
		i <- i + 1

		if (substring(line, 1, 3) == "###") {
			sp.line <- strsplit(line, split = " ")
			parts <- sp.line[[1]]
			atom.cnt <- parts[2]
			pdbcode <- parts[4]

			if (pdbcode != pdbcode0) {
				if (pdbcode0 != "") {
					listPDB[[pdbcode0]] <- flexList
				}

				flexList <- list()
				pdbcode0 <- pdbcode
			}

			#cat(pdbcode0, " | ", pdbcode, " | ", parts, "\n")
			fileIdx <- parts[5]
			plen <- length(parts)
			flexName <- paste(parts[6:plen], collapse = "_")
			chain <- NULL
			resNum <- NULL
			atomType <- NULL
			resType <- NULL
			xco <- NULL
			yco <- NULL
			zco <- NULL

			for (j in 1:as.numeric(atom.cnt)) {
				line <- fdata[i]
				i <- i + 1
				sp.line <- strsplit(line, split = " ")
				parts <- sp.line[[1]]
				chain <- c(chain, parts[1])
				resNum <- c(resNum, as.numeric(parts[2]))
				atomType <- c(atomType, parts[3])
				resType <- c(resType, parts[4])
				xco <- c(xco, as.numeric(parts[5]))
				yco <- c(yco, as.numeric(parts[6]))
				zco <- c(zco, as.numeric(parts[7]))
			}

			flex0 <- data.frame(chain, resNum, atomType, resType, xco, yco, zco, 
				stringsAsFactors = F)
			#cat("Assign flexons: ", pdbcode, flexName, "\n")
			flexList[[flexName]] <- flex0
		}
	}

	# got to pick up the last flexons
	if (length(pdbcode0) > 0) {
		listPDB[[pdbcode0]] <- flexList
	}

	return(listPDB)

} # end read.flexons

