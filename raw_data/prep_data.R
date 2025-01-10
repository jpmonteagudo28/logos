library(dplyr)
library(tidyr)
library(arrow)

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
  colnames(file_data)[1] <- "Text"

  # Process the data
  processed_files[[i]] <- file_data |>
    as.data.frame() |>
    tibble::rownames_to_column(var = "Reference") |>
    mutate(Reference = trimws(Reference)) |>
    separate(Reference, into = c("Book", "ChapterVerse"), sep = " ", fill = "right") |>
    separate(ChapterVerse, into = c("Chapter", "Verse"), sep = ":", fill = "right") |>
    mutate(greek_name = greek_name)
}

# Combine all processed data into a single data frame
new_testament <- bind_rows(processed_files) |>
  select(1:5)

saveRDS(new_testament,file = "data/new_testament")


#---- --- ---- --- ---- --- ---- --- ---- --- ----#
# Retrieved from Logos Bible Software,
# Introduction to the New Testament
# https://wesleyhuff.com
# The Making of the New Testament
# Bible Study Tools https://www.biblestudytools.com/bible-study/topical-studies/who-wrote-the-bible.html

# Author data
# Create Bible data frame with 66 entries (one per book)
bible_data <- data.frame(
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

saveRDS(bible_data,file = "data/author_data")
#---- --- ---- --- ---- --- ---- --- ---- --- ----#
