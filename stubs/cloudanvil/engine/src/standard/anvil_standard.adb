-- SPDX-License-Identifier: AGPL-3.0-or-later
-- SPDX-FileCopyrightText: 2025 hyperpolymath
--
-- anvil-ssg: Standard (non-SPARK) implementation
-- Merged from noteg-ssg - full markdown parsing, templating, and rendering

with Ada.Text_IO;
with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Characters.Latin_1;

package body Anvil_Standard is

   use Ada.Text_IO;
   use Ada.Directories;
   use Ada.Strings.Fixed;

   LF : constant Character := Ada.Characters.Latin_1.LF;

   ---------------------
   -- Create_Site --
   ---------------------

   function Create_Site (Name : String; Base_URL : String) return Site is
   begin
      return (Name       => To_Unbounded_String (Name),
              Base_URL   => To_Unbounded_String (Base_URL),
              Output_Dir => To_Unbounded_String ("_site"),
              Pages      => Page_Vectors.Empty_Vector);
   end Create_Site;

   ----------------
   -- Add_Page --
   ----------------

   procedure Add_Page (S : in out Site; P : Page) is
   begin
      S.Pages.Append (P);
   end Add_Page;

   ----------------------
   -- Parse_Markdown --
   ----------------------

   function Parse_Markdown (Content : String) return Block_Vectors.Vector is
      Result : Block_Vectors.Vector;
      Lines  : constant String := Content;
      Start  : Positive := Lines'First;
      Finish : Natural;
   begin
      while Start <= Lines'Last loop
         -- Find end of line
         Finish := Index (Lines (Start .. Lines'Last), "" & LF);
         if Finish = 0 then
            Finish := Lines'Last;
         else
            Finish := Finish - 1;
         end if;

         declare
            Line : constant String := Lines (Start .. Finish);
            Block : Content_Block;
         begin
            if Line'Length > 0 then
               -- Parse headers
               if Line (Line'First) = '#' then
                  Block.Kind := Header;
                  if Line'Length > 1 and then Line (Line'First + 1) = '#' then
                     if Line'Length > 2 and then Line (Line'First + 2) = '#' then
                        Block.Level := 3;
                        Block.Text := To_Unbounded_String (Trim (Line (Line'First + 3 .. Line'Last), Ada.Strings.Both));
                     else
                        Block.Level := 2;
                        Block.Text := To_Unbounded_String (Trim (Line (Line'First + 2 .. Line'Last), Ada.Strings.Both));
                     end if;
                  else
                     Block.Level := 1;
                     Block.Text := To_Unbounded_String (Trim (Line (Line'First + 1 .. Line'Last), Ada.Strings.Both));
                  end if;

               -- Parse code blocks (simple)
               elsif Line'Length >= 3 and then Line (Line'First .. Line'First + 2) = "```" then
                  Block.Kind := Code;
                  Block.Text := To_Unbounded_String ("");

               -- Parse list items
               elsif Line (Line'First) = '*' or else Line (Line'First) = '-' then
                  Block.Kind := List_Item;
                  Block.Text := To_Unbounded_String (Trim (Line (Line'First + 1 .. Line'Last), Ada.Strings.Both));

               -- Parse quotes
               elsif Line (Line'First) = '>' then
                  Block.Kind := Quote;
                  Block.Text := To_Unbounded_String (Trim (Line (Line'First + 1 .. Line'Last), Ada.Strings.Both));

               -- Default: paragraph
               else
                  Block.Kind := Paragraph;
                  Block.Text := To_Unbounded_String (Line);
               end if;

               Result.Append (Block);
            end if;
         end;

         Start := Finish + 2;  -- Skip LF
      end loop;

      return Result;
   end Parse_Markdown;

   -------------------
   -- Escape_HTML --
   -------------------

   function Escape_HTML (Text : String) return String is
      Result : Unbounded_String := To_Unbounded_String ("");
   begin
      for C of Text loop
         case C is
            when '<' => Append (Result, "&lt;");
            when '>' => Append (Result, "&gt;");
            when '&' => Append (Result, "&amp;");
            when '"' => Append (Result, "&quot;");
            when others => Append (Result, C);
         end case;
      end loop;
      return To_String (Result);
   end Escape_HTML;

   -----------------------
   -- Markdown_To_HTML --
   -----------------------

   function Markdown_To_HTML (Blocks : Block_Vectors.Vector) return String is
      Result : Unbounded_String := To_Unbounded_String ("");
   begin
      for Block of Blocks loop
         case Block.Kind is
            when Header =>
               Append (Result, "<h" & Trim (Block.Level'Image, Ada.Strings.Both) & ">");
               Append (Result, Escape_HTML (To_String (Block.Text)));
               Append (Result, "</h" & Trim (Block.Level'Image, Ada.Strings.Both) & ">" & LF);

            when Paragraph =>
               Append (Result, "<p>" & Escape_HTML (To_String (Block.Text)) & "</p>" & LF);

            when Code =>
               Append (Result, "<pre><code>" & Escape_HTML (To_String (Block.Text)) & "</code></pre>" & LF);

            when List_Item =>
               Append (Result, "<li>" & Escape_HTML (To_String (Block.Text)) & "</li>" & LF);

            when Quote =>
               Append (Result, "<blockquote>" & Escape_HTML (To_String (Block.Text)) & "</blockquote>" & LF);
         end case;
      end loop;

      return To_String (Result);
   end Markdown_To_HTML;

   -------------------
   -- Render_Page --
   -------------------

   function Render_Page (P : Page; Template : String) return String is
      pragma Unreferenced (Template);  -- TODO: Implement template substitution
      Content_HTML : constant String := Markdown_To_HTML (P.Content);
   begin
      return "<!DOCTYPE html>" & LF &
             "<html lang=""en"">" & LF &
             "<head>" & LF &
             "  <meta charset=""UTF-8"">" & LF &
             "  <title>" & Escape_HTML (To_String (P.Meta.Title)) & "</title>" & LF &
             "</head>" & LF &
             "<body>" & LF &
             "  <article>" & LF &
             "    <h1>" & Escape_HTML (To_String (P.Meta.Title)) & "</h1>" & LF &
             Content_HTML &
             "  </article>" & LF &
             "</body>" & LF &
             "</html>" & LF;
   end Render_Page;

   ------------------
   -- Build_Site --
   ------------------

   procedure Build_Site (S : Site) is
      Out_Dir : constant String := To_String (S.Output_Dir);
   begin
      -- Create output directory
      if not Exists (Out_Dir) then
         Create_Directory (Out_Dir);
      end if;

      Put_Line ("anvil-ssg: Building site '" & To_String (S.Name) & "'...");
      Put_Line ("anvil-ssg: Output directory: " & Out_Dir);

      -- Build each page
      for P of S.Pages loop
         declare
            HTML     : constant String := Render_Page (P, "");
            Out_Path : constant String := Out_Dir & "/" &
                       To_String (P.Meta.Path) & ".html";
         begin
            Write_HTML (Out_Path, HTML);
            Put_Line ("anvil-ssg: Built " & Out_Path);
         end;
      end loop;

      Put_Line ("anvil-ssg: Built " & S.Pages.Length'Image & " pages");
   end Build_Site;

   --------------------------
   -- Parse_Front_Matter --
   --------------------------

   function Parse_Front_Matter (Content : String) return Page_Metadata is
      Result : Page_Metadata;
   begin
      -- Simple YAML-like front matter parsing
      -- TODO: Implement proper parsing
      Result.Title := To_Unbounded_String ("Untitled");
      Result.Path := To_Unbounded_String ("index");
      Result.Template := To_Unbounded_String ("default");
      pragma Unreferenced (Content);
      return Result;
   end Parse_Front_Matter;

   -----------------
   -- Load_Page --
   -----------------

   function Load_Page (Path : String) return Page is
      Result : Page;
      File   : File_Type;
      Content : Unbounded_String := To_Unbounded_String ("");
   begin
      Open (File, In_File, Path);
      while not End_Of_File (File) loop
         Append (Content, Get_Line (File) & LF);
      end loop;
      Close (File);

      Result.Meta := Parse_Front_Matter (To_String (Content));
      Result.Content := Parse_Markdown (To_String (Content));
      return Result;
   end Load_Page;

   ------------------
   -- Write_HTML --
   ------------------

   procedure Write_HTML (Path : String; Content : String) is
      File : File_Type;
   begin
      Create (File, Out_File, Path);
      Put (File, Content);
      Close (File);
   end Write_HTML;

end Anvil_Standard;
