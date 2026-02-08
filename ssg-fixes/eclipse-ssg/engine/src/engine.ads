-- SPDX-License-Identifier: AGPL-3.0-or-later
-- SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
-- engine.ads — Eclipse SSG Core Engine (Ada/SPARK)
--
-- This is the core engine specification following Ada 2012/SPARK principles.
-- Provides formal verification guarantees for SSG operations.

pragma SPARK_Mode (On);

package Engine is

   -- Version information
   Version : constant String := "0.2.0";

   -- Maximum sizes for verification
   Max_Path_Length    : constant := 4096;
   Max_Content_Length : constant := 1_048_576;  -- 1MB
   Max_Template_Vars  : constant := 256;

   -- Status codes
   type Status_Code is (
      Success,
      Error_File_Not_Found,
      Error_Parse_Failed,
      Error_Template_Invalid,
      Error_Build_Failed,
      Error_Verification_Failed
   );

   -- Content types
   type Content_Format is (
      Markdown,
      AsciiDoc,
      Org_Mode,
      Plain_Text
   );

   -- Build modes
   type Build_Mode is (
      Development,
      Production,
      Verification
   );

   -- Build configuration
   type Build_Config is record
      Mode        : Build_Mode;
      Output_Dir  : String (1 .. Max_Path_Length);
      Output_Len  : Natural;
      Include_Drafts : Boolean;
      Minify      : Boolean;
   end record;

   -- Initialize the engine
   procedure Initialize
     with Global => null,
          Post   => Is_Initialized;

   -- Check if engine is initialized
   function Is_Initialized return Boolean
     with Global => null;

   -- Build a site
   procedure Build (Config : in Build_Config; Result : out Status_Code)
     with Global => null,
          Pre    => Is_Initialized,
          Post   => Result in Success | Error_Build_Failed;

   -- Verify build output
   procedure Verify_Output (Path : in String; Result : out Status_Code)
     with Global => null,
          Pre    => Is_Initialized and Path'Length <= Max_Path_Length;

   -- Clean build artifacts
   procedure Clean (Output_Dir : in String; Result : out Status_Code)
     with Global => null,
          Pre    => Output_Dir'Length <= Max_Path_Length;

end Engine;
