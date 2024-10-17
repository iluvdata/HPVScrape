# HPVScrape for R

## Introduction

This is a tool use to scrape Xpert HPV pdf reports and return the data as a formatted data frame of S3 class `xpert_results`.

## Usage

### Case 1: Xpert machine(s) all in the same time zone

To parse a pdf (which can contain multiple results) simple create a new `Xpert` object. You can pass either a path or a raw vector of the pdf binary.

``` r
# Use OlsonNames() for a list of valid time zones

xpert <- Xpert("myXpertResults.pdf", "America/New_York")
```

### Case 2: Multiple Xpert machines in various time zones

To parse a pdf first create a `TZ` object mapping the Xpert Instrument Serial Numbers to correct time zones.

``` r
tz <- TZ$new("110016067" = "America/Sao_Paulo", "110016068" = "America/Mexico_City")

xpert <- Xpert("myXpertResults.pdf", tz)
```

### Get Results

``` r
myResults <- xpert$getResults()
```

### Get PDF for a result

To get the PDF for a row in the data frame returned from `Xpert$getResults()` just pass the row number to `Xpert$getPDFs()`. The second argument is a path to the output file

``` r
pathToPDF <- xpert$getPDFs(2, "myResults.pdf")
```

## License

GPL-3
