-- SPDX-License-Identifier: AGPL-3.0-or-later
-- SPDX-FileCopyrightText: 2025 hyperpolymath
--
-- anvil-ssg: Standard (non-SPARK) implementation
-- Merged from noteg-ssg for full markdown parsing, templating, and rendering
--
-- This package provides the feature-complete SSG engine in Ada,
-- complementing the SPARK-verified core with full functionality.

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

package Anvil_Standard is

   -- Page metadata with strong typing
   type Page_Kind is (Article, Index, Archive, Custom);

   type Page_Metadata is record
      Title       : Unbounded_String;
      Path        : Unbounded_String;
      Template    : Unbounded_String;
      Kind        : Page_Kind := Article;
      Draft       : Boolean := False;
      Date        : Unbounded_String;
   end record;

   -- Content representation
   type Content_Block_Kind is (Paragraph, Header, Code, List_Item, Quote);

   type Content_Block is record
      Kind    : Content_Block_Kind;
      Text    : Unbounded_String;
      Level   : Natural := 1;  -- For headers
   end record;

   package Block_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Content_Block);

   -- A page is metadata plus content blocks
   type Page is record
      Meta    : Page_Metadata;
      Content : Block_Vectors.Vector;
   end record;

   package Page_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Page);

   -- Site representation
   type Site is record
      Name        : Unbounded_String;
      Base_URL    : Unbounded_String;
      Output_Dir  : Unbounded_String;
      Pages       : Page_Vectors.Vector;
   end record;

   -- Core operations
   function Create_Site (Name : String; Base_URL : String) return Site;

   procedure Add_Page (S : in out Site; P : Page);

   function Parse_Markdown (Content : String) return Block_Vectors.Vector;

   function Render_Page (P : Page; Template : String) return String;

   procedure Build_Site (S : Site);

   -- File operations
   function Load_Page (Path : String) return Page;

   procedure Write_HTML (Path : String; Content : String);

   -- Front matter parsing
   function Parse_Front_Matter (Content : String) return Page_Metadata;

private

   -- Internal helpers
   function Escape_HTML (Text : String) return String;
   function Markdown_To_HTML (Blocks : Block_Vectors.Vector) return String;

end Anvil_Standard;
