# Install and load required libraries
if (!require(pdftools)) install.packages("pdftools")
if (!require(tesseract)) install.packages("tesseract")
library(pdftools)
library(tesseract)
library(stringr)

# Define paths for Deepnote
pdf_folder <- "R:/Masterarbeit/wef/wef2015"
output_text_folder <- "R:/Masterarbeit/wef/wef_text"

# Create output directory if it doesn't exist
dir.create(output_text_folder, showWarnings = FALSE, recursive = TRUE)

# Function to extract text from PDFs with OCR fallback
extract_text_with_fallback <- function(pdf_path) {
  text <- tryCatch({
    pdf_text(pdf_path)
  }, error = function(e) {
    message("Regular text extraction failed. Switching to OCR for: ", pdf_path)
    return(NULL)
  })

  if (is.null(text) || length(text) == 0 || all(is.na(text))) {
    pages <- pdf_convert(pdf_path, dpi = 300)
    text <- sapply(pages, function(img) tesseract::ocr(img))
    unlink(pages)  # Clean up temporary images
  }

  return(text)
}

# Main function to process PDFs
process_pdfs <- function(pdf_folder, output_text_folder) {
  pdf_files <- list.files(pdf_folder, pattern = "\\.pdf$", full.names = TRUE)

  for (pdf_path in pdf_files) {
    # Generate a base name for the output files
    pdf_name <- tools::file_path_sans_ext(basename(pdf_path))
    pdf_name <- str_replace_all(pdf_name, "[^A-Za-z0-9_-]", "_")

    message("Processing PDF: ", pdf_name)

    # Extract text from each page
    text_pages <- extract_text_with_fallback(pdf_path)

    # Save text for each page
    for (page_num in seq_along(text_pages)) {
      page_text <- text_pages[[page_num]]
      text_lines <- unlist(strsplit(page_text, "\n"))

      # Save text
      if (any(nzchar(trimws(text_lines)))) {
        txt_file <- file.path(output_text_folder, paste0(pdf_name, "_page_", page_num, ".txt"))
        writeLines(trimws(text_lines), txt_file)
        message("Text saved to: ", txt_file)
      }
    }
  }
}

# Run the main processing function
process_pdfs(pdf_folder, output_text_folder)
