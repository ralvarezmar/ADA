-- $Id: misc_util.ads,v 1.1 1997/02/17 23:13:06 jgb Exp $
--

package Misc_Util is

   generic
      First_String:  in String;
      Second_String: in String;
      type Object1 is private;
      type Object2 is private;
      with function Image1 (An_Object: in Object1) return String is <>;
      with function Image2 (An_Object: in Object2) return String is <>;
   function Image (First_Object:  in Object1;
		   Second_Object: in Object2)
		   return String;

end Misc_Util;
