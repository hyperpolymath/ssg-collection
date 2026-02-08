      * SPDX-License-Identifier: AGPL-3.0-or-later
      * SPDX-FileCopyrightText: 2025 hyperpolymath
      *
      * Division One SSG - COBOL Static Site Generator
      * "Let divisions organize. Let records structure. Let pages compile."
      *
      * Compile with: cobc -x -o divisionone src/divisionone.cob
      *
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DIVISIONONE-SSG.
       AUTHOR. HYPERPOLYMATH.
       DATE-WRITTEN. 2025-01-18.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           FUNCTION ALL INTRINSIC.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CONTENT-FILE ASSIGN TO WS-CONTENT-PATH
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.
           SELECT OUTPUT-FILE ASSIGN TO WS-OUTPUT-PATH
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD CONTENT-FILE.
       01 CONTENT-RECORD                 PIC X(1024).

       FD OUTPUT-FILE.
       01 OUTPUT-RECORD                  PIC X(1024).

       WORKING-STORAGE SECTION.
      * Site Configuration
       01 WS-SITE-CONFIG.
          05 WS-SITE-TITLE               PIC X(80)
                                         VALUE "Division One Site".
          05 WS-SITE-AUTHOR              PIC X(40)
                                         VALUE "COBOL Developer".
          05 WS-OUTPUT-DIR               PIC X(120)
                                         VALUE "_site".
          05 WS-CONTENT-DIR              PIC X(120)
                                         VALUE "content".

      * File handling
       01 WS-FILE-STATUS                 PIC XX.
       01 WS-CONTENT-PATH                PIC X(256).
       01 WS-OUTPUT-PATH                 PIC X(256).
       01 WS-EOF-FLAG                    PIC 9 VALUE 0.
          88 END-OF-FILE                 VALUE 1.

      * Page metadata (frontmatter)
       01 WS-PAGE-META.
          05 WS-PAGE-TITLE               PIC X(200).
          05 WS-PAGE-DATE                PIC X(20).
          05 WS-PAGE-DRAFT               PIC 9 VALUE 0.
             88 IS-DRAFT                 VALUE 1.

      * Content processing
       01 WS-LINE                        PIC X(1024).
       01 WS-LINE-LENGTH                 PIC 9(4).
       01 WS-IN-FRONTMATTER              PIC 9 VALUE 0.
          88 IN-FRONTMATTER              VALUE 1.
       01 WS-FRONTMATTER-DELIM           PIC 9 VALUE 0.
       01 WS-IN-CODE-BLOCK               PIC 9 VALUE 0.
          88 IN-CODE-BLOCK               VALUE 1.

      * HTML generation
       01 WS-HTML-BUFFER                 PIC X(4096).
       01 WS-BODY-BUFFER                 PIC X(32000).
       01 WS-BODY-LENGTH                 PIC 9(5) VALUE 0.

      * Command line
       01 WS-COMMAND                     PIC X(20).
       01 WS-ARG1                        PIC X(256).
       01 WS-PAGE-COUNT                  PIC 9(4) VALUE 0.

      * Working variables
       01 WS-I                           PIC 9(4).
       01 WS-CHAR                        PIC X.
       01 WS-TEMP                        PIC X(1024).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM PARSE-ARGUMENTS
           EVALUATE WS-COMMAND
               WHEN "build"
                   PERFORM BUILD-SITE
               WHEN "init"
                   PERFORM INIT-SITE
               WHEN "clean"
                   PERFORM CLEAN-SITE
               WHEN "help"
                   PERFORM SHOW-HELP
               WHEN OTHER
                   PERFORM SHOW-HELP
           END-EVALUATE
           STOP RUN.

       PARSE-ARGUMENTS.
           ACCEPT WS-COMMAND FROM COMMAND-LINE
           IF WS-COMMAND = SPACES
               MOVE "help" TO WS-COMMAND
           END-IF.

       BUILD-SITE.
           DISPLAY "divisionone-ssg: Building site..."
           DISPLAY "  Output directory: " WS-OUTPUT-DIR

      * Create output directory
           STRING "mkdir -p " DELIMITED SIZE
                  WS-OUTPUT-DIR DELIMITED SPACE
                  INTO WS-TEMP
           END-STRING
           CALL "SYSTEM" USING WS-TEMP

      * Process content files (simplified - single file for demo)
           STRING WS-CONTENT-DIR DELIMITED SPACE
                  "/index.md" DELIMITED SIZE
                  INTO WS-CONTENT-PATH
           END-STRING

           STRING WS-OUTPUT-DIR DELIMITED SPACE
                  "/index.html" DELIMITED SIZE
                  INTO WS-OUTPUT-PATH
           END-STRING

           PERFORM PROCESS-MARKDOWN-FILE

           DISPLAY "divisionone-ssg: Built " WS-PAGE-COUNT " pages"
           DISPLAY "divisionone-ssg: Build complete.".

       PROCESS-MARKDOWN-FILE.
           OPEN INPUT CONTENT-FILE
           IF WS-FILE-STATUS NOT = "00"
               DISPLAY "  No content found at " WS-CONTENT-PATH
               EXIT PARAGRAPH
           END-IF

           INITIALIZE WS-PAGE-META
           INITIALIZE WS-BODY-BUFFER
           MOVE 0 TO WS-BODY-LENGTH
           MOVE 0 TO WS-IN-FRONTMATTER
           MOVE 0 TO WS-FRONTMATTER-DELIM
           MOVE 0 TO WS-IN-CODE-BLOCK

      * Read and process content
           PERFORM UNTIL END-OF-FILE
               READ CONTENT-FILE INTO WS-LINE
                   AT END
                       SET END-OF-FILE TO TRUE
                   NOT AT END
                       PERFORM PROCESS-LINE
               END-READ
           END-PERFORM

           CLOSE CONTENT-FILE

      * Generate HTML output
           PERFORM GENERATE-HTML-PAGE
           ADD 1 TO WS-PAGE-COUNT.

       PROCESS-LINE.
           MOVE FUNCTION LENGTH(FUNCTION TRIM(WS-LINE))
               TO WS-LINE-LENGTH

      * Check for frontmatter delimiter
           IF WS-LINE(1:3) = "---"
               ADD 1 TO WS-FRONTMATTER-DELIM
               IF WS-FRONTMATTER-DELIM = 1
                   SET IN-FRONTMATTER TO TRUE
               ELSE
                   MOVE 0 TO WS-IN-FRONTMATTER
               END-IF
               EXIT PARAGRAPH
           END-IF

      * Parse frontmatter
           IF IN-FRONTMATTER
               PERFORM PARSE-FRONTMATTER-LINE
               EXIT PARAGRAPH
           END-IF

      * Parse markdown content
           PERFORM PARSE-MARKDOWN-LINE.

       PARSE-FRONTMATTER-LINE.
           IF WS-LINE(1:6) = "title:"
               MOVE FUNCTION TRIM(WS-LINE(7:))
                   TO WS-PAGE-TITLE
           END-IF
           IF WS-LINE(1:5) = "date:"
               MOVE FUNCTION TRIM(WS-LINE(6:))
                   TO WS-PAGE-DATE
           END-IF
           IF WS-LINE(1:6) = "draft:"
               IF WS-LINE(7:4) = "true"
                   SET IS-DRAFT TO TRUE
               END-IF
           END-IF.

       PARSE-MARKDOWN-LINE.
      * Code blocks
           IF WS-LINE(1:3) = "```"
               IF IN-CODE-BLOCK
                   STRING WS-BODY-BUFFER DELIMITED "  "
                          "</code></pre>" DELIMITED SIZE
                          X"0A" DELIMITED SIZE
                          INTO WS-BODY-BUFFER
                   END-STRING
                   MOVE 0 TO WS-IN-CODE-BLOCK
               ELSE
                   STRING WS-BODY-BUFFER DELIMITED "  "
                          "<pre><code>" DELIMITED SIZE
                          INTO WS-BODY-BUFFER
                   END-STRING
                   SET IN-CODE-BLOCK TO TRUE
               END-IF
               EXIT PARAGRAPH
           END-IF

           IF IN-CODE-BLOCK
               STRING WS-BODY-BUFFER DELIMITED "  "
                      WS-LINE DELIMITED "  "
                      X"0A" DELIMITED SIZE
                      INTO WS-BODY-BUFFER
               END-STRING
               EXIT PARAGRAPH
           END-IF

      * Headers
           IF WS-LINE(1:4) = "### "
               STRING WS-BODY-BUFFER DELIMITED "  "
                      "<h3>" DELIMITED SIZE
                      FUNCTION TRIM(WS-LINE(5:)) DELIMITED "  "
                      "</h3>" DELIMITED SIZE
                      X"0A" DELIMITED SIZE
                      INTO WS-BODY-BUFFER
               END-STRING
               EXIT PARAGRAPH
           END-IF

           IF WS-LINE(1:3) = "## "
               STRING WS-BODY-BUFFER DELIMITED "  "
                      "<h2>" DELIMITED SIZE
                      FUNCTION TRIM(WS-LINE(4:)) DELIMITED "  "
                      "</h2>" DELIMITED SIZE
                      X"0A" DELIMITED SIZE
                      INTO WS-BODY-BUFFER
               END-STRING
               EXIT PARAGRAPH
           END-IF

           IF WS-LINE(1:2) = "# "
               STRING WS-BODY-BUFFER DELIMITED "  "
                      "<h1>" DELIMITED SIZE
                      FUNCTION TRIM(WS-LINE(3:)) DELIMITED "  "
                      "</h1>" DELIMITED SIZE
                      X"0A" DELIMITED SIZE
                      INTO WS-BODY-BUFFER
               END-STRING
               EXIT PARAGRAPH
           END-IF

      * List items
           IF WS-LINE(1:2) = "- " OR WS-LINE(1:2) = "* "
               STRING WS-BODY-BUFFER DELIMITED "  "
                      "<li>" DELIMITED SIZE
                      FUNCTION TRIM(WS-LINE(3:)) DELIMITED "  "
                      "</li>" DELIMITED SIZE
                      X"0A" DELIMITED SIZE
                      INTO WS-BODY-BUFFER
               END-STRING
               EXIT PARAGRAPH
           END-IF

      * Empty line
           IF WS-LINE-LENGTH = 0
               EXIT PARAGRAPH
           END-IF

      * Paragraph
           STRING WS-BODY-BUFFER DELIMITED "  "
                  "<p>" DELIMITED SIZE
                  FUNCTION TRIM(WS-LINE) DELIMITED "  "
                  "</p>" DELIMITED SIZE
                  X"0A" DELIMITED SIZE
                  INTO WS-BODY-BUFFER
           END-STRING.

       GENERATE-HTML-PAGE.
           OPEN OUTPUT OUTPUT-FILE
           IF WS-FILE-STATUS NOT = "00"
               DISPLAY "  Error creating " WS-OUTPUT-PATH
               EXIT PARAGRAPH
           END-IF

      * Write HTML header
           MOVE "<!DOCTYPE html>" TO OUTPUT-RECORD
           WRITE OUTPUT-RECORD
           MOVE "<html lang='en'>" TO OUTPUT-RECORD
           WRITE OUTPUT-RECORD
           MOVE "<head>" TO OUTPUT-RECORD
           WRITE OUTPUT-RECORD
           MOVE "<meta charset='UTF-8'>" TO OUTPUT-RECORD
           WRITE OUTPUT-RECORD

           STRING "<title>" DELIMITED SIZE
                  FUNCTION TRIM(WS-PAGE-TITLE) DELIMITED "  "
                  "</title>" DELIMITED SIZE
                  INTO OUTPUT-RECORD
           END-STRING
           WRITE OUTPUT-RECORD

           MOVE "<style>" TO OUTPUT-RECORD
           WRITE OUTPUT-RECORD
           MOVE "body{font-family:monospace;max-width:80ch;" TO
               OUTPUT-RECORD
           WRITE OUTPUT-RECORD
           MOVE "margin:0 auto;padding:2rem;}" TO OUTPUT-RECORD
           WRITE OUTPUT-RECORD
           MOVE "pre{background:#f0f0f0;padding:1rem;}" TO OUTPUT-RECORD
           WRITE OUTPUT-RECORD
           MOVE "</style>" TO OUTPUT-RECORD
           WRITE OUTPUT-RECORD
           MOVE "</head>" TO OUTPUT-RECORD
           WRITE OUTPUT-RECORD
           MOVE "<body>" TO OUTPUT-RECORD
           WRITE OUTPUT-RECORD
           MOVE "<article>" TO OUTPUT-RECORD
           WRITE OUTPUT-RECORD

      * Write title
           STRING "<h1>" DELIMITED SIZE
                  FUNCTION TRIM(WS-PAGE-TITLE) DELIMITED "  "
                  "</h1>" DELIMITED SIZE
                  INTO OUTPUT-RECORD
           END-STRING
           WRITE OUTPUT-RECORD

      * Write date
           IF WS-PAGE-DATE NOT = SPACES
               STRING "<time>" DELIMITED SIZE
                      FUNCTION TRIM(WS-PAGE-DATE) DELIMITED "  "
                      "</time>" DELIMITED SIZE
                      INTO OUTPUT-RECORD
               END-STRING
               WRITE OUTPUT-RECORD
           END-IF

      * Write body content
           MOVE WS-BODY-BUFFER TO OUTPUT-RECORD
           WRITE OUTPUT-RECORD

      * Write HTML footer
           MOVE "</article>" TO OUTPUT-RECORD
           WRITE OUTPUT-RECORD
           MOVE "<footer>" TO OUTPUT-RECORD
           WRITE OUTPUT-RECORD
           MOVE "<p>Generated by Division One SSG (COBOL)</p>" TO
               OUTPUT-RECORD
           WRITE OUTPUT-RECORD
           MOVE "</footer>" TO OUTPUT-RECORD
           WRITE OUTPUT-RECORD
           MOVE "</body>" TO OUTPUT-RECORD
           WRITE OUTPUT-RECORD
           MOVE "</html>" TO OUTPUT-RECORD
           WRITE OUTPUT-RECORD

           CLOSE OUTPUT-FILE
           DISPLAY "  Written: " WS-OUTPUT-PATH.

       INIT-SITE.
           DISPLAY "divisionone-ssg: Initializing new site..."

      * Create directories
           STRING "mkdir -p " DELIMITED SIZE
                  WS-CONTENT-DIR DELIMITED SPACE
                  INTO WS-TEMP
           END-STRING
           CALL "SYSTEM" USING WS-TEMP

           STRING "mkdir -p " DELIMITED SIZE
                  WS-OUTPUT-DIR DELIMITED SPACE
                  INTO WS-TEMP
           END-STRING
           CALL "SYSTEM" USING WS-TEMP

      * Create sample content
           STRING WS-CONTENT-DIR DELIMITED SPACE
                  "/index.md" DELIMITED SIZE
                  INTO WS-OUTPUT-PATH
           END-STRING

           OPEN OUTPUT OUTPUT-FILE
           MOVE "---" TO OUTPUT-RECORD
           WRITE OUTPUT-RECORD
           MOVE "title: Welcome to Division One" TO OUTPUT-RECORD
           WRITE OUTPUT-RECORD
           MOVE "date: 2025-01-18" TO OUTPUT-RECORD
           WRITE OUTPUT-RECORD
           MOVE "---" TO OUTPUT-RECORD
           WRITE OUTPUT-RECORD
           MOVE SPACES TO OUTPUT-RECORD
           WRITE OUTPUT-RECORD
           MOVE "# Welcome" TO OUTPUT-RECORD
           WRITE OUTPUT-RECORD
           MOVE SPACES TO OUTPUT-RECORD
           WRITE OUTPUT-RECORD
           MOVE "This site was built with **Division One SSG**." TO
               OUTPUT-RECORD
           WRITE OUTPUT-RECORD
           MOVE SPACES TO OUTPUT-RECORD
           WRITE OUTPUT-RECORD
           MOVE "## COBOL Features" TO OUTPUT-RECORD
           WRITE OUTPUT-RECORD
           MOVE SPACES TO OUTPUT-RECORD
           WRITE OUTPUT-RECORD
           MOVE "- Self-documenting syntax" TO OUTPUT-RECORD
           WRITE OUTPUT-RECORD
           MOVE "- Record-oriented processing" TO OUTPUT-RECORD
           WRITE OUTPUT-RECORD
           MOVE "- Division architecture" TO OUTPUT-RECORD
           WRITE OUTPUT-RECORD
           MOVE "- Enterprise reliability" TO OUTPUT-RECORD
           WRITE OUTPUT-RECORD
           CLOSE OUTPUT-FILE

           DISPLAY "  Created " WS-CONTENT-DIR "/"
           DISPLAY "  Created sample content"
           DISPLAY "divisionone-ssg: Site initialized.".

       CLEAN-SITE.
           DISPLAY "divisionone-ssg: Cleaning " WS-OUTPUT-DIR "/"
           STRING "rm -rf " DELIMITED SIZE
                  WS-OUTPUT-DIR DELIMITED SPACE
                  INTO WS-TEMP
           END-STRING
           CALL "SYSTEM" USING WS-TEMP
           DISPLAY "divisionone-ssg: Clean complete.".

       SHOW-HELP.
           DISPLAY "Division One SSG - COBOL Static Site Generator"
           DISPLAY " "
           DISPLAY "USAGE:"
           DISPLAY "  divisionone <command>"
           DISPLAY " "
           DISPLAY "COMMANDS:"
           DISPLAY "  build    Build the site"
           DISPLAY "  init     Initialize a new site"
           DISPLAY "  clean    Remove generated files"
           DISPLAY "  help     Show this help"
           DISPLAY " "
           DISPLAY "EXAMPLES:"
           DISPLAY "  divisionone init"
           DISPLAY "  divisionone build".
