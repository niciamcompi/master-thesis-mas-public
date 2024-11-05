# Install and load required libraries
if (!require(pdftools)) install.packages("pdftools")
if (!require(tesseract)) install.packages("tesseract")
if (!require(stringr)) install.packages("stringr")
library(pdftools)
library(tesseract)
library(stringr)

# Define paths
pdf_folder <- "C:/Users/nici_/OneDrive/Desktop/Masterarbeit/Factsheets"
output_text_folder <- "C:/Users/nici_/OneDrive/Desktop/Masterarbeit/Factsheets/Text"
output_tables_folder <- "C:/Users/nici_/OneDrive/Desktop/Masterarbeit/Factsheets/Tables"

# Create output directories if they don't exist
dir.create(output_text_folder, showWarnings = FALSE, recursive = TRUE)
dir.create(output_tables_folder, showWarnings = FALSE, recursive = TRUE)

# Define company names to be anonymized
company_names <- c("Vontobel", "Nomura", "iShares", "Stiftung für den flexiblen Altersrücktritt im Bauhauptgewerbe", "VAM", "AssetManagement")

# Initialize company anonymization data
company_index <- list()
company_count <- 0

# Function to anonymize sensitive information in text
anonymize_text <- function(text) {
  text <- vapply(text, function(line) {
    # Anonymize emails
    line <- str_replace_all(line, "\\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}\\b", "[EMAIL]")
    
    # Anonymize names (capitalized first and last names)
    line <- str_replace_all(line, "\\b([A-Z][a-z]+\\s[A-Z][a-z]+)\\b", "[NAME]")
    
    # Enhanced Address Anonymization: catch embedded keywords within words
    address_keywords <- paste(c(
      "Strasse", "platz", "weg", "gasse", "allee", "ring", "chemin", "rue", "route"
    ), collapse = "|")
    address_pattern <- paste0("(?i)\\b\\d{1,5}\\s*[A-Za-z]*(", address_keywords, ")|[A-Za-z]*(", address_keywords, ")\\b")
    line <- str_replace_all(line, address_pattern, "[ADDRESS]")
    
    # Anonymize Swiss phone numbers
    line <- str_replace_all(line, "\\+41\\s?\\d{2}\\s?\\d{3}\\s?\\d{2}\\s?\\d{2}", "[PHONE]")
    
    # Anonymize Swiss postcodes and remove following city/village names
    line <- str_replace_all(line, "\\b([3-9]\\d{3})\\b\\s+\\w+", "[POSTCODE]")
    
    # Anonymize company names
    for (company in company_names) {
      pattern <- paste0("(?i)", company)
      if (grepl(pattern, line, perl = TRUE)) {
        if (!company %in% names(company_index)) {
          company_count <<- company_count + 1
          company_index[[company]] <<- paste0("[COMPANY_", company_count, "]")
        }
        line <- str_replace_all(line, pattern, company_index[[company]])
      }
    }
    return(line)
  }, character(1))
  
  return(text)
}


# Function to anonymize company names in filenames
anonymize_filename <- function(filename) {
  for (company in company_names) {
    pattern <- paste0("(?i)", company)
    if (grepl(pattern, filename)) {
      if (!company %in% names(company_index)) {
        company_count <<- company_count + 1
        company_index[[company]] <<- paste0("[COMPANY_", company_count, "]")
      }
      filename <- str_replace_all(filename, pattern, company_index[[company]])
    }
  }
  return(filename)
}

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

# Function to detect and extract table-like data from text
extract_table_like_rows <- function(text_lines) {
  # Detect lines that resemble tables based on numeric patterns and spacing
  table_lines <- text_lines[grepl("^(.*?\\d+.*?\\s{2,}.*?\\d+.*)+$", text_lines)]
  
  if (length(table_lines) > 0) {
    message("Detected potential table rows:")
    print(table_lines)  # Print for debugging purposes
    
    # Split lines into columns, ensuring consistent number of columns
    table_data <- strsplit(table_lines, "\\s{2,}")
    max_columns <- max(sapply(table_data, length))
    table_data <- lapply(table_data, function(row) {
      length(row) <- max_columns
      row[is.na(row)] <- ""
      return(row)
    })
    
    return(table_data)
  } else {
    message("No table-like rows detected.")
    return(NULL)
  }
}

# Main function to process PDFs and extract text and tables
process_pdfs <- function(pdf_folder, output_text_folder, output_tables_folder) {
  pdf_files <- list.files(pdf_folder, pattern = "\\.pdf$", full.names = TRUE)
  
  for (pdf_path in pdf_files) {
    # Anonymize filename
    pdf_name <- tools::file_path_sans_ext(basename(pdf_path))
    pdf_name <- anonymize_filename(pdf_name)
    
    message("Processing PDF: ", pdf_name)
    
    # Extract text from each page
    text_pages <- extract_text_with_fallback(pdf_path)
    
    # Process each page for text and tables
    for (page_num in seq_along(text_pages)) {
      page_text <- text_pages[[page_num]]
      text_lines <- unlist(strsplit(page_text, "\n"))
      
      # Extract tables and exclude them from text output if any
      table_data <- extract_table_like_rows(text_lines)
      if (!is.null(table_data)) {
        # Anonymize table data
        table_data <- lapply(table_data, anonymize_text)
        
        # Save table as a txt file
        table_file <- file.path(output_tables_folder, paste0(pdf_name, "_page_", page_num, "_table.txt"))
        writeLines(unlist(lapply(table_data, paste, collapse = "\t")), table_file)
        message("Table saved to: ", table_file)
        
        # Remove table lines from text output
        text_lines <- text_lines[!text_lines %in% unlist(lapply(table_data, paste, collapse = "\t"))]
      } else {
        message("No tables found on page: ", page_num)
      }
      
      # Anonymize remaining text lines
      anonymized_text <- anonymize_text(text_lines)
      
      # Save anonymized text (if there is content)
      if (any(nzchar(trimws(anonymized_text)))) {
        left_aligned_text <- trimws(anonymized_text, which = "left")
        txt_file <- file.path(output_text_folder, paste0(pdf_name, "_page_", page_num, ".txt"))
        writeLines(left_aligned_text, txt_file)
        message("Anonymized text saved to: ", txt_file)
      }
    }
  }
}

# Run the main processing function
process_pdfs(pdf_folder, output_text_folder, output_tables_folder)
