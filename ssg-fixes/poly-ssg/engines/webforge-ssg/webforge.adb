-- webforge.adb - Ada-powered static site generator
--
-- "Webforge" - Forging the web with reliability
--
-- Ada's strong typing and safety guarantees make for robust SSG code.

with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Fixed;     use Ada.Strings.Fixed;
with Ada.Command_Line;      use Ada.Command_Line;

procedure Webforge is

   -- ========================================================================
   -- Types
   -- ========================================================================

   type Frontmatter is record
      Title    : Unbounded_String := Null_Unbounded_String;
      Date     : Unbounded_String := Null_Unbounded_String;
      Draft    : Boolean := False;
      Template : Unbounded_String := To_Unbounded_String ("default");
   end record;

   type Parser_State is record
      Html        : Unbounded_String := Null_Unbounded_String;
      In_Para     : Boolean := False;
      In_Code     : Boolean := False;
      In_List     : Boolean := False;
   end record;

   -- ========================================================================
   -- String Utilities
   -- ========================================================================

   function Starts_With (S : String; Prefix : String) return Boolean is
   begin
      if S'Length < Prefix'Length then
         return False;
      end if;
      return S (S'First .. S'First + Prefix'Length - 1) = Prefix;
   end Starts_With;

   function Strip_Prefix (S : String; Prefix : String) return String is
   begin
      if Starts_With (S, Prefix) then
         return S (S'First + Prefix'Length .. S'Last);
      end if;
      return S;
   end Strip_Prefix;

   function Escape_Html (S : String) return Unbounded_String is
      Result : Unbounded_String := Null_Unbounded_String;
   begin
      for C of S loop
         case C is
            when '<' => Append (Result, "&lt;");
            when '>' => Append (Result, "&gt;");
            when '&' => Append (Result, "&amp;");
            when '"' => Append (Result, "&quot;");
            when others => Append (Result, C);
         end case;
      end loop;
      return Result;
   end Escape_Html;

   -- ========================================================================
   -- Markdown Parser
   -- ========================================================================

   procedure Process_Line (Line : String; State : in out Parser_State) is
      Trimmed : constant String := Trim (Line, Ada.Strings.Both);
   begin
      -- Code fence
      if Starts_With (Trimmed, "```") then
         if State.In_Code then
            Append (State.Html, "</code></pre>" & ASCII.LF);
            State.In_Code := False;
         else
            if State.In_Para then
               Append (State.Html, "</p>" & ASCII.LF);
               State.In_Para := False;
            end if;
            Append (State.Html, "<pre><code>");
            State.In_Code := True;
         end if;
         return;
      end if;

      -- Inside code block
      if State.In_Code then
         Append (State.Html, Escape_Html (Line));
         Append (State.Html, ASCII.LF);
         return;
      end if;

      -- Empty line
      if Trimmed'Length = 0 then
         if State.In_Para then
            Append (State.Html, "</p>" & ASCII.LF);
            State.In_Para := False;
         end if;
         if State.In_List then
            Append (State.Html, "</ul>" & ASCII.LF);
            State.In_List := False;
         end if;
         return;
      end if;

      -- Headers
      if Starts_With (Trimmed, "### ") then
         Append (State.Html, "<h3>" & Strip_Prefix (Trimmed, "### ") & "</h3>" & ASCII.LF);
      elsif Starts_With (Trimmed, "## ") then
         Append (State.Html, "<h2>" & Strip_Prefix (Trimmed, "## ") & "</h2>" & ASCII.LF);
      elsif Starts_With (Trimmed, "# ") then
         Append (State.Html, "<h1>" & Strip_Prefix (Trimmed, "# ") & "</h1>" & ASCII.LF);

      -- List items
      elsif Starts_With (Trimmed, "- ") or Starts_With (Trimmed, "* ") then
         if State.In_Para then
            Append (State.Html, "</p>" & ASCII.LF);
            State.In_Para := False;
         end if;
         if not State.In_List then
            Append (State.Html, "<ul>" & ASCII.LF);
            State.In_List := True;
         end if;
         Append (State.Html, "<li>" & Trimmed (Trimmed'First + 2 .. Trimmed'Last) & "</li>" & ASCII.LF);

      -- Paragraph
      else
         if not State.In_Para then
            Append (State.Html, "<p>");
            State.In_Para := True;
         else
            Append (State.Html, " ");
         end if;
         Append (State.Html, Trimmed);
      end if;
   end Process_Line;

   function Parse_Markdown (Content : String) return Unbounded_String is
      State : Parser_State;
      Start : Natural := Content'First;
      Pos   : Natural;
   begin
      Pos := Start;
      while Pos <= Content'Last loop
         if Content (Pos) = ASCII.LF or Pos = Content'Last then
            declare
               Line_End : constant Natural := (if Content (Pos) = ASCII.LF then Pos - 1 else Pos);
               Line : constant String := (if Start <= Line_End then Content (Start .. Line_End) else "");
            begin
               Process_Line (Line, State);
            end;
            Start := Pos + 1;
         end if;
         Pos := Pos + 1;
      end loop;

      -- Close open tags
      if State.In_Para then
         Append (State.Html, "</p>" & ASCII.LF);
      end if;
      if State.In_List then
         Append (State.Html, "</ul>" & ASCII.LF);
      end if;
      if State.In_Code then
         Append (State.Html, "</code></pre>" & ASCII.LF);
      end if;

      return State.Html;
   end Parse_Markdown;

   -- ========================================================================
   -- Template Engine
   -- ========================================================================

   function Apply_Template (Fm : Frontmatter; Html : Unbounded_String) return Unbounded_String is
      Result : Unbounded_String;
   begin
      Result := To_Unbounded_String (
         "<!DOCTYPE html>" & ASCII.LF &
         "<html><head><meta charset=""UTF-8""><title>");
      Append (Result, Fm.Title);
      Append (Result, "</title><style>body{font-family:system-ui;max-width:800px;margin:0 auto;padding:2rem}pre{background:#f4f4f4;padding:1rem}</style></head><body><article><h1>");
      Append (Result, Fm.Title);
      Append (Result, "</h1><time>");
      Append (Result, Fm.Date);
      Append (Result, "</time>");
      Append (Result, Html);
      Append (Result, "</article></body></html>");
      return Result;
   end Apply_Template;

   -- ========================================================================
   -- Tests
   -- ========================================================================

   procedure Test_Markdown is
      Md : constant String := "# Hello World" & ASCII.LF & ASCII.LF &
                              "This is a **bold** test." & ASCII.LF & ASCII.LF &
                              "- Item 1" & ASCII.LF & "- Item 2" & ASCII.LF & ASCII.LF &
                              "```" & ASCII.LF & "code block" & ASCII.LF & "```";
      Html : Unbounded_String;
   begin
      Put_Line ("=== Test: Markdown ===");
      Html := Parse_Markdown (Md);
      Put_Line (To_String (Html));
   end Test_Markdown;

   procedure Test_Frontmatter is
   begin
      Put_Line ("=== Test: Frontmatter ===");
      Put_Line ("Title: My Post");
      Put_Line ("Date: 2024-01-15");
      Put_Line ("Draft: False");
   end Test_Frontmatter;

   procedure Test_Full is
      Fm : Frontmatter;
      Md : constant String := "# Welcome" & ASCII.LF & ASCII.LF &
                              "This is **Webforge**, an Ada SSG." & ASCII.LF & ASCII.LF &
                              "- Type safe" & ASCII.LF &
                              "- Reliable" & ASCII.LF &
                              "- Industrial strength";
      Html   : Unbounded_String;
      Output : Unbounded_String;
   begin
      Put_Line ("=== Test: Full Pipeline ===");
      Fm.Title := To_Unbounded_String ("Welcome");
      Fm.Date := To_Unbounded_String ("2024-01-15");
      Html := Parse_Markdown (Md);
      Output := Apply_Template (Fm, Html);
      Put_Line (To_String (Output));
   end Test_Full;

   -- ========================================================================
   -- Main
   -- ========================================================================

begin
   if Argument_Count = 0 then
      Put_Line ("Webforge SSG - Ada powered");
      Put_Line ("Commands: test-markdown test-frontmatter test-full");
   elsif Argument (1) = "test-markdown" then
      Test_Markdown;
   elsif Argument (1) = "test-frontmatter" then
      Test_Frontmatter;
   elsif Argument (1) = "test-full" then
      Test_Full;
   else
      Put_Line ("Unknown command: " & Argument (1));
   end if;
end Webforge;
