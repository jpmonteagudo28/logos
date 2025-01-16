library(dplyr)
library(tidyr)
library(stringr)

# New Testament files
files <- files <- list.files("raw_data/sblgnt/text", full.names = TRUE)
processed_files <- list()

# Loop over the files
for (i in seq_along(files)) {
  # Read the file
  file_data <- read.delim(files[i], header = TRUE, sep = "\t")

  # Debugging: Check the file data
  print(head(file_data))  # Ensure the file is being read correctly

  # Extract the header name
  greek_name <- colnames(file_data)[1]
  colnames(file_data)[1] <- "text"

  # Process the data
  processed_files[[i]] <- file_data |>
    as.data.frame() |>
    tibble::rownames_to_column(var = "Reference") |>
    mutate(Reference = trimws(Reference)) |>
    separate(Reference, into = c("book", "ChapterVerse"), sep = " ", fill = "right") |>
    separate(ChapterVerse, into = c("chapter", "verse"), sep = ":", fill = "right") |>
    mutate(greek_name = greek_name,
           chapter = as.numeric(chapter),
           verse = as.numeric(verse))
}

# Combine all processed data into a single data frame
new_testament <- bind_rows(processed_files) |>
  select(1:5)

usethis::use_data(new_testament)


#---- --- ---- --- ---- --- ---- --- ---- --- ----#
# Retrieved from Logos Bible Software,
# Introduction to the New Testament
# https://wesleyhuff.com
# The Making of the New Testament
# Bible Study Tools https://www.biblestudytools.com/bible-study/topical-studies/who-wrote-the-bible.html

# Author data
# Create Bible data frame with 66 entries (one per book)
author_data <- data.frame(
  author = c(
    # Old Testament - 39 books
    "Moses",           #1  Genesis
    "Moses",           #2  Exodus
    "Moses",           #3  Leviticus
    "Moses",           #4  Numbers
    "Moses",           #5  Deuteronomy
    "Joshua",          #6  Joshua
    "Unknown",         #7  Judges
    "Unknown",         #8  Ruth
    "Samuel/Unknown",  #9  1 Samuel
    "Samuel/Unknown",  #10 2 Samuel
    "Unknown",         #11 1 Kings
    "Unknown",         #12 2 Kings
    "Unknown",         #13 1 Chronicles
    "Unknown",         #14 2 Chronicles
    "Ezra",           #15 Ezra
    "Nehemiah",       #16 Nehemiah
    "Unknown",         #17 Esther
    "Unknown",         #18 Job
    "David/Various",   #19 Psalms
    "Solomon",         #20 Proverbs
    "Solomon",         #21 Ecclesiastes
    "Solomon",         #22 Song of Solomon
    "Isaiah",         #23 Isaiah
    "Jeremiah",       #24 Jeremiah
    "Jeremiah",       #25 Lamentations
    "Ezekiel",        #26 Ezekiel
    "Daniel",         #27 Daniel
    "Hosea",          #28 Hosea
    "Joel",           #29 Joel
    "Amos",           #30 Amos
    "Obadiah",        #31 Obadiah
    "Jonah",          #32 Jonah
    "Micah",          #33 Micah
    "Nahum",          #34 Nahum
    "Habakkuk",       #35 Habakkuk
    "Zephaniah",      #36 Zephaniah
    "Haggai",         #37 Haggai
    "Zechariah",      #38 Zechariah
    "Malachi",        #39 Malachi
    # New Testament - 27 books
    "Matthew",        #40 Matthew
    "Mark",           #41 Mark
    "Luke",           #42 Luke
    "John",           #43 John
    "Luke",           #44 Acts
    "Paul",           #45 Romans
    "Paul",           #46 1 Corinthians
    "Paul",           #47 2 Corinthians
    "Paul",           #48 Galatians
    "Paul",           #49 Ephesians
    "Paul",           #50 Philippians
    "Paul",           #51 Colossians
    "Paul",           #52 1 Thessalonians
    "Paul",           #53 2 Thessalonians
    "Paul",           #54 1 Timothy
    "Paul",           #55 2 Timothy
    "Paul",           #56 Titus
    "Paul",           #57 Philemon
    "Unknown",        #58 Hebrews
    "James",          #59 James
    "Peter",          #60 1 Peter
    "Peter",          #61 2 Peter
    "John",           #62 1 John
    "John",           #63 2 John
    "John",           #64 3 John
    "Jude",           #65 Jude
    "John"            #66 Revelation
  ),
  books = c(
    # Old Testament
    "Genesis", "Exodus", "Leviticus", "Numbers", "Deuteronomy",
    "Joshua", "Judges", "Ruth", "1 Samuel", "2 Samuel",
    "1 Kings", "2 Kings", "1 Chronicles", "2 Chronicles",
    "Ezra", "Nehemiah", "Esther", "Job", "Psalms", "Proverbs",
    "Ecclesiastes", "Song of Solomon", "Isaiah", "Jeremiah",
    "Lamentations", "Ezekiel", "Daniel", "Hosea", "Joel",
    "Amos", "Obadiah", "Jonah", "Micah", "Nahum", "Habakkuk",
    "Zephaniah", "Haggai", "Zechariah", "Malachi",
    # New Testament
    "Matthew", "Mark", "Luke", "John", "Acts",
    "Romans", "1 Corinthians", "2 Corinthians", "Galatians",
    "Ephesians", "Philippians", "Colossians", "1 Thessalonians",
    "2 Thessalonians", "1 Timothy", "2 Timothy", "Titus",
    "Philemon", "Hebrews", "James", "1 Peter", "2 Peter",
    "1 John", "2 John", "3 John", "Jude", "Revelation"
  ),
  section = c(
    # Old Testament
    rep("Law", 5),                    #1-5
    rep("History", 12),               #6-17
    "Wisdom",                         #18
    rep("Wisdom", 4),                 #19-22
    rep("Major Prophets", 5),         #23-27
    rep("Minor Prophets", 12),        #28-39
    # New Testament
    rep("Gospels", 4),               #40-43
    "History",                       #44
    rep("Pauline Epistles", 13),     #45-57
    rep("General Epistles", 8),      #58-65
    "Apocalyptic"                    #66
  ),
  date = c(
    # Old Testament dates
    rep("c. 1445-1405 BC", 5),      #Pentateuch
    "c. 1405-1385 BC",              #Joshua
    "c. 1043 BC",                   #Judges
    "c. 1030-1010 BC",              #Ruth
    rep("c. 931-722 BC", 2),        #Samuel
    rep("c. 561-538 BC", 2),        #Kings
    rep("c. 450-430 BC", 2),        #Chronicles
    "c. 457-444 BC",                #Ezra
    "c. 424-400 BC",                #Nehemiah
    "c. 450-331 BC",                #Esther
    "Unknown",                      #Job
    "c. 1410-450 BC",               #Psalms
    "c. 971-686 BC",                #Proverbs
    "c. 940-931 BC",                #Ecclesiastes
    "c. 971-965 BC",                #Song of Solomon
    "c. 700-681 BC",                #Isaiah
    "c. 586-570 BC",                #Jeremiah
    "c. 586 BC",                    #Lamentations
    "c. 590-570 BC",                #Ezekiel
    "c. 636-530 BC",                #Daniel
    "c. 750-710 BC",                #Hosea
    "c. 835-796 BC",                #Joel
    "c. 760-750 BC",                #Amos
    "c. 850-840 BC",                #Obadiah
    "c. 775 BC",                    #Jonah
    "c. 735-710 BC",                #Micah
    "c. 650 BC",                    #Nahum
    "c. 615-605 BC",                #Habakkuk
    "c. 635-625 BC",                #Zephaniah
    "c. 520 BC",                    #Haggai
    "c. 480-470 BC",                #Zechariah
    "c. 433-424 BC",                #Malachi
    # New Testament dates
    "c. 50-60 AD",                  #Matthew
    "c. 50-60 AD",                  #Mark
    "c. 60-61 AD",                  #Luke
    "c. 80-90 AD",                  #John
    "c. 61-62 AD",                  #Acts
    "c. 56 AD",                     #Romans
    "c. 55 AD",                     #1 Corinthians
    "c. 55-56 AD",                  #2 Corinthians
    "c. 49-50 AD",                  #Galatians
    "c. 60-62 AD",                  #Ephesians
    "c. 60-62 AD",                  #Philippians
    "c. 60-62 AD",                  #Colossians
    "c. 51 AD",                     #1 Thessalonians
    "c. 51-52 AD",                  #2 Thessalonians
    "c. 62-64 AD",                  #1 Timothy
    "c. 66-67 AD",                  #2 Timothy
    "c. 62-64 AD",                  #Titus
    "c. 60-62 AD",                  #Philemon
    "c. 67-69 AD",                  #Hebrews
    "c. 44-49 AD",                  #James
    "c. 64-65 AD",                  #1 Peter
    "c. 67-68 AD",                  #2 Peter
    "c. 90-95 AD",                  #1 John
    "c. 90-95 AD",                  #2 John
    "c. 90-95 AD",                  #3 John
    "c. 68-70 AD",                     #Jude
    "c. 94-96 AD"                   #Revelation
  ),
  testament = c(
    rep("Old Testament", 39),
    rep("New Testament", 27)
  ),
  language = c(
    rep("Hebrew", 39),
    rep("Greek", 27)
  )
)

usethis::use_data(author_data)

#---- --- ---- --- ---- --- ---- --- ---- --- ----#
# Leningrad Codex obtained from Tanach Inc.
# https://www.tanach.us/Pages/Technical.html#zipfiles

book_abbreviations <- c(
  "Genesis" = "Gen",
  "Exodus" = "Exod",
  "Leviticus" = "Lev",
  "Numbers" = "Num",
  "Deuteronomy" = "Deut",
  "Joshua" = "Josh",
  "Judges" = "Judg",
  "Samuel_1" = "1Sam",
  "Samuel_2" = "2Sam",
  "Kings_1" = "1Kgs",
  "Kings_2" = "2Kgs",
  "Isaiah" = "Isa",
  "Jeremiah" = "Jer",
  "Ezekiel" = "Ezek",
  "Hosea" = "Hos",
  "Joel" = "Joel",
  "Amos" = "Amos",
  "Obadiah" = "Obad",
  "Jonah" = "Jonah",
  "Micah" = "Mic",
  "Nahum" = "Nah",
  "Habakkuk" = "Hab",
  "Zephaniah" = "Zeph",
  "Haggai" = "Hag",
  "Zechariah" = "Zech",
  "Malachi" = "Mal",
  "Psalms" = "Ps",
  "Proverbs" = "Prov",
  "Job" = "Job",
  "Song_of_Songs" = "Song",
  "Ruth" = "Ruth",
  "Lamentations" = "Lam",
  "Ecclesiastes" = "Eccl",
  "Esther" = "Esth",
  "Daniel" = "Dan",
  "Ezra" = "Ezra",
  "Nehemiah" = "Neh",
  "Chronicles_1" = "1Chron",
  "Chronicles_2" = "2Chron"
)

hebrew_to_english <- c(
  "בְּרֵאשִׁית" = "Genesis",
  "שְׁמֹות" = "Exodus",
  "וַיִּקְרָא" = "Leviticus",
  "בְּמִדְבַּר" = "Numbers",
  "דְּבָרִים" = "Deuteronomy",
  "יְהוֹשֻעַ" = "Joshua",
  "שֹׁפְטִים" = "Judges",
  "שְׁמוּאֵל" = "Samuel",
  "מְלָכִים" = "Kings",
  "יְשַׁעְיָהוּ" = "Isaiah",
  "יִרְמְיָהוּ" = "Jeremiah",
  "יְחֶזְקֵאל" = "Ezekiel",
  "הוֹשֵׁעַ" = "Hosea",
  "יוֹאֵל" = "Joel",
  "עָמוֹס" = "Amos",
  "עֹבַדְיָה" = "Obadiah",
  "יוֹנָה" = "Jonah",
  "מִיכָה" = "Micah",
  "נַחוּם" = "Nahum",
  "חֲבַקּוּק" = "Habakkuk",
  "צְפַנְיָה" = "Zephaniah",
  "חַגַּי" = "Haggai",
  "זְכַרְיָה" = "Zechariah",
  "מַלְאָכִי" = "Malachi",
  "תְהִלִּים" = "Psalms",
  "מִשְׁלֵי" = "Proverbs",
  "אִיּוֹב" = "Job",
  "שִׁיר הַשִּׁירִים" = "Song of Songs",
  "רוּת" = "Ruth",
  "אֵיכָה" = "Lamentations",
  "קֹהֶלֶת" = "Ecclesiastes",
  "אֶסְתֵר" = "Esther",
  "דָּנִיֵּאל" = "Daniel",
  "עֶזְרָא" = "Ezra",
  "נְחֶמְיָה" = "Nehemiah",
  "דִּבְרֵי הַיָּמִים" = "Chronicles"
) |> as.data.frame() |>
  tibble::rownames_to_column(var = "hebrew_name") |>
  rename("english_equivalent" =
              starts_with("c("))

hebrew_to_english <- lapply(hebrew_to_english,
                            function(col) sort(col)) |>
  as.data.frame()

# Removing comments from each text file

# Define the directory containing the files
directory_path <- "raw_data/leningrad_codex/text"

# Get a list of all text files in the directory
file_list <- list.files(directory_path,
                        pattern = "\\.txt$",
                        full.names = TRUE)

# Function to handle bidi characters in regex
bidi_safe_grepl <- function(pattern, text) {
  grepl(pattern, text, perl = TRUE) # Enable Perl-compatible regex for advanced patterns
}

# Update your loop:
for (file_path in file_list) {
  # Read the file
  lines <- readLines(file_path, encoding = "UTF-8")

  # Filter out lines starting with `xxxx` (ignoring bidi formatting)
  cleaned_lines <- lines[!bidi_safe_grepl("^xxxx", lines)]

  # Write the cleaned content back to the file
  writeLines(cleaned_lines, file_path, useBytes = TRUE)

  cat("Processed file:", file_path, "\n")
}

format_book_name <- function(filename) {
  # Remove file extension
  basename <- tools::file_path_sans_ext(filename)

  # Look up the full name in our abbreviations
  abbrev <- book_abbreviations[basename]

  # Return the abbreviation or original name if not found
  if (!is.na(abbrev)) {
    return(abbrev)
  } else {
    warning(sprintf("No abbreviation found for book: %s", basename))
    return(basename)
  }
}


process_hebrew_text <- function(text_lines) {
  # Split the text into lines and create a dataframe
  df <- data.frame(
    raw_text = text_lines,
    stringsAsFactors = FALSE
  ) |>
    # Extract verse numbers and chapter numbers
    mutate(
      # Extract the numbers before ׃
      verse_chapter = str_extract(raw_text, "^\\s*\\d+\\s*׃\\s*\\d+"),
      # Clean up the actual text
      text = str_replace(raw_text, "^\\s*\\d+\\s*׃\\s*\\d+\\s*", "")
    ) |>
    # Separate verse and chapter
    separate(
      verse_chapter,
      into = c("verse", "chapter"),
      sep = "׃",
      fill = "right"
    ) |>
    # Clean up numbers
    mutate(
      verse = as.numeric(str_trim(verse)),
      chapter = as.numeric(str_trim(chapter)),
      # Remove any remaining whitespace from text
      text = str_trim(text)
    )

  return(df)
}


process_leningrad_codex <- function(file_path) {
  # Read the file
  text_lines <- readLines(file_path, encoding = "UTF-8")

  # Process the text
  processed_df <- process_hebrew_text(text_lines)

  # Add book information (you might want to extract this from filename or pass as parameter)
  processed_df$book <- format_book_name(basename(file_path))

  return(processed_df)
}

# To process multiple files:
process_all_files <- function(directory_path) {
  # Get all files in directory
  files <- list.files(directory_path, full.names = TRUE, pattern = "\\.txt$")

  # Process each file
  processed_files <- lapply(files, process_leningrad_codex)

  # Combine all files
  full_codex <- bind_rows(processed_files)

  # Reorder columns
  full_codex <- full_codex |>
    select(book, chapter, verse, text)

  return(full_codex)
}

old_testament <- process_all_files("raw_data/leningrad_codex/text")

ot <- old_testament |>
  mutate(book = str_remove_all(book, "^\\d")) |> # Remove numbers from the book names
  group_by(book) |>
  count() |>
  ungroup() |>
  arrange(book)

count <- ot$n
hebrew_names <- rep(hebrew_to_english$hebrew_name,times = count)

old_testament <- append_column(old_testament,
                        hebrew_names,
                        .after = "text")


usethis::use_data(old_testament)

#---- --- ---- --- ---- --- ---- --- ---- --- ---- --- ----#
# Creating RASV data frame

directory_path <- "raw_data/rasv"

# Get a list of all text files in the directory
file <- list.files(directory_path,
                        pattern = "\\.db$",
                        full.names = TRUE)

file_data <- read.delim(file, header = FALSE)

# Debugging: Check the file data
print(head(file_data))  # Ensure the file is being read correctly

# Extract the header name
colnames(file_data)[1] <- "Text"

# Process the data
processed_file <- file_data |>
  as.data.frame() |>
  rename(raw = everything()) |>
  separate(raw, into = c("book", "chapter", "verse", "text"),
           sep = "\\|") |>
  mutate(
    chapter = as.integer(chapter),
    verse = as.integer(verse),
    book = first_to_upper(book)
  )

verse_counts <- processed_file |>
  dplyr::group_by(book) |>
  dplyr::count()|>
  dplyr::ungroup()

bible_books <- c(
  "1Chr", "1Cor", "1John", "1Kgs", "1Pet", "1Sam", "1Thess", "1Tim",
  "2Chr", "2Cor", "2John", "2Kgs", "2Pet", "2Sam", "2Thess", "2Tim",
  "3John", "Acts", "Amos", "Col", "Dan", "Deut", "Ecc", "Eph", "Est",
  "Exod", "Ezek", "Ezra", "Gal", "Gen", "Hab", "Hag", "Heb", "Hos",
  "Isa", "Jude", "Jdg", "Jer", "John", "James", "Jon", "Job", "Joel",
  "Josh", "Lam", "Lev", "Luke", "Mal", "Mic", "Mark", "Matt", "Nah",
  "Neh", "Num", "Obad", "Phil", "Phlm", "Prov", "Psms", "Rev", "Rom",
  "Ruth", "Song", "Titus", "Zech", "Zeph"
)

verses_by_book <- verse_counts |>
  dplyr::mutate(
    book = bible_books
  )

# Total number of verses checks out for OT and NT

rasb_bible <- processed_file

usethis::use_data(rasb_bible)

usethis::use_data(verses_by_book)
