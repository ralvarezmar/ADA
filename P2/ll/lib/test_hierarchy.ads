-- $Id: test_hierarchy.ads,v 1.4 1998/01/19 12:47:28 jgb Exp $
--

-- This package provides a hierarchy of types rooted at
--  type Root, which must be abstract, and which is a
--  parameter of the generic.
--

-- Used in private part, for getting some test strings.
--
with Test_Strings_Instantiated;

generic
   -- Root type of the provided hierarchy.
   type Root is abstract tagged private;
package Test_Hierarchy is

   pragma Elaborate_Body;

   -- Derived type, able of storing a string of one character.
   --
   type Derived_1 is new Root with private;

   -- Get a string from a derived type.
   --
   function Image (A_Derived_1: in Derived_1) return String;

   -- Get a derived type from a string.
   --
   function To_Derived (A_String: in String) return Derived_1;


   -- Derived type, able of storing a string of 5 characters.
   --
   type Derived_5 is new Root with private;

   -- Get a string from a derived type.
   --
   function Image (A_Derived_5: in Derived_5) return String;

   -- Get a derived type from a string.
   --
   function To_Derived (A_String: in String) return Derived_5;


   -- Derived type, able of storing a string of 10 character   --
   type Derived_10 is new Root with private;

   -- Get a string from a derived type.
   --
   function Image (A_Derived_10: in Derived_10) return String;

   -- Get a derived type from a string.
   --
   function To_Derived (A_String: in String) return Derived_10;


   -- Derived type, able of storing a string of 600 characters.
   --
   type Derived_600 is new Root with private;

   -- Get a string from a derived type.
   --
   function Image (A_Derived_600: in Derived_600) return String;

   -- Get a derived type from a string.
   --
   function To_Derived (A_String: in String) return Derived_600;

   -- Derived type, able of storing a string of 600 characters.
   --
   type Derived_1024 is new Root with private;

   -- Get a string from a derived type.
   --
   function Image (A_Derived_1024: in Derived_1024) return String;

   -- Get a derived type from a string.
   --
   function To_Derived (A_String: in String) return Derived_1024;

private

   package TStrings renames Test_Strings_Instantiated;

   type Derived_1 is new Root with
      record
         Data: TStrings.Test_String_1;
      end record;

   type Derived_5 is new Root with
      record
         Data: TStrings.Test_String_5;
      end record;

   type Derived_10 is new Root with
      record
         Data: TStrings.Test_String_10;
      end record;

   type Derived_600 is new Root with
      record
         Data: TStrings.Test_String_600;
      end record;

   type Derived_1024 is new Root with
      record
         Data: TStrings.Test_String_1024;
      end record;

end Test_Hierarchy;
