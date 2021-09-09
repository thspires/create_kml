create_kml <- function(dataframe, filename) {
  # Creates a kml file from a data.frame that contains coordinates in decimal degree
  # No elevation is allowed
  # objeto data.frame com campos 'lat', 'lon' e 'sp'
  if (class(dataframe) != "data.frame") {
    print("First argument must be a valid object of class data.frame")
  }
  if (!("lon" %in% names(dataframe))) {
    print("Data frame must contain a longitude column called lon")
    return()
  }
  if (!("lat" %in% names(dataframe))) {
    print("Data frame must contain a longitude column called lat")
    return()
  }
  if (!("sp" %in% names(dataframe))) {
    print("Data frame must contain species name column called sp")
    return()
  }
  # Checar se está em grau decimal com regex
  # TODO
  # Nome do arquivo de destino (append .kml se nao for explicito)
  if (grepl(".kml",filename) == F) {
    filename = paste0(filename,".kml")
  }
  sink(file=filename)
  cat('<?xml version="1.0" encoding="UTF-8"?>
    <kml xmlns="http://www.opengis.net/kml/2.2" xmlns:gx="http://www.google.com/kml/ext/2.2" xmlns:kml="http://www.opengis.net/kml/2.2" xmlns:atom="http://www.w3.org/2005/Atom">
    <Document>')
  for (i in 1:nrow(kmldata)) {
    coords <- paste(dataframe$lon[i], dataframe$lat[i], "0", sep=",")
    sp <- as.character(dataframe$sp[i])
    cat('
      <Placemark>
        <name>',sp,'</name>
        <styleUrl>#m_ylw-pushpin</styleUrl>
        <Point>
          <gx:drawOrder>1</gx:drawOrder>
          <coordinates>', coords, '</coordinates>
        </Point>
      </Placemark>
        ')
  }
  cat('
  </Document>
    </kml>')
  sink()
}
