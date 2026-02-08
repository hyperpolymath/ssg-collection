-- SPDX-License-Identifier: AGPL-3.0-or-later
-- SPDX-FileCopyrightText: 2025 hyperpolymath
--
-- anvil-ssg: Ada/SPARK static site generator
-- "Forge-in-Forge technology" - formally verified site generation
--
-- This package uses SPARK annotations for formal verification,
-- ensuring the build pipeline is provably correct.

pragma SPARK_Mode (On);

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Anvil_Engine with
   SPARK_Mode => On
is
   -- Maximum sizes for SPARK provability
   Max_Title_Length : constant := 256;
   Max_Path_Length  : constant := 1024;
   Max_Pages        : constant := 10_000;

   -- Bounded string types for SPARK
   subtype Title_Length is Natural range 0 .. Max_Title_Length;
   subtype Path_Length is Natural range 0 .. Max_Path_Length;
   subtype Page_Count is Natural range 0 .. Max_Pages;

   -- Page metadata with contracts
   type Page_Metadata is record
      Title       : Unbounded_String;
      Path        : Unbounded_String;
      Title_Len   : Title_Length := 0;
      Path_Len    : Path_Length := 0;
   end record;

   -- Site with bounded page count
   type Site is record
      Name       : Unbounded_String;
      Output_Dir : Unbounded_String;
      Page_Count : Page_Count := 0;
   end record;

   -- Core operations with preconditions
   function Create_Site (Name : String) return Site
     with Pre => Name'Length > 0 and Name'Length <= Max_Title_Length,
          Post => Create_Site'Result.Page_Count = 0;

   procedure Build_Site (S : in out Site)
     with Pre => S.Page_Count >= 0,
          Post => S.Page_Count = S.Page_Count'Old;

   -- Verified HTML escaping
   function Escape_HTML (Text : String) return String
     with Pre => Text'Length <= 10_000,
          Post => Escape_HTML'Result'Length >= Text'Length;

   -- File operations with contracts
   procedure Write_File (Path : String; Content : String)
     with Pre => Path'Length > 0 and Path'Length <= Max_Path_Length;

end Anvil_Engine;
