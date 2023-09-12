# Uncomment the following lines if the packages are not installed
# install.packages("bitops")
# install.packages("base64enc")

library(bitops)
library(base64enc)


# Function to convert GNUBG Position ID to XGID position string
convert_position_id <- function(gnubg_position_id) {
  # Decode the Base64 encoded Position ID
  decoded_bytes <- base64decode(gnubg_position_id)

  # Initialize empty position string for XGID
  xgid_position <- ""

  # Loop through each byte and convert to bits
  for(byte in decoded_bytes) {
    # Get 8-bit binary representation of the byte
    bits <- intToBits(byte)

    # Loop through the bits to construct the position string
    # In GNUBG, a '1' represents a checker and a '0' separates points
    ones_count <- 0
    for(bit in bits) {
      if(bit) {
        ones_count <- ones_count + 1
      } else {
        if(ones_count > 0) {
          xgid_position <- paste0(xgid_position, LETTERS[ones_count])
        } else {
          xgid_position <- paste0(xgid_position, "-")
        }
        ones_count <- 0
      }
    }
  }

  return(xgid_position)
}


# Function to convert GNUBG Match ID to XGID match information
convert_match_id <- function(gnubg_match_id) {
  # Decode the Base64 encoded Match ID
  decoded_bytes <- base64decode(gnubg_match_id)

  # Convert bytes to bits and parse according to GNUBG documentation
  # For simplification, directly using byte values for this example

  cube_value_byte <- bitAnd(decoded_bytes[1], as.integer(15))  # First 4 bits
  cube_value <- 2 ^ cube_value_byte

  cube_owner_byte <- bitAnd(bitShiftR(decoded_bytes[1], 4), as.integer(3))  # Bits 5-6
  cube_position <- ifelse(cube_owner_byte == 3, 0, cube_owner_byte)

  # Other fields can be parsed similarly

  # Create the XGID match string
  xgid_match <- paste0(":", cube_value, ":", cube_position, ":1:00:0:0:0:0:0")

  return(xgid_match)
}



# Example usage
gnubg_position_id <- "4HPwATDgc/ABMA"
gnubg_match_id <- "QYkqASAAIAAA"

xgid_position <- convert_position_id(gnubg_position_id)
xgid_match <- convert_match_id(gnubg_match_id)

xgid <- paste0("XGID=", xgid_position, xgid_match)

print(paste0("Converted XGID: ", xgid))


devtools::load_all()
ggboard(xgid)
