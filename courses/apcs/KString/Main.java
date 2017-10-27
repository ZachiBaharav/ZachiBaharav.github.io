
public class Main {
    
    public static void main(String[] args)
    {
        
        // String is a Class !!
        // Standard one in Java
        String str = "Hello John Smith";
        System.out.println(str);
        
        // There are a few methods which we will use.
        // The ones given on the AP card at the back:
        /*
        class java.lang.String
        int length()  
        String substring(int from, int to) - returns the substring beginning at from and ending at to-1
        String substring(int from) - returns substring(from, length())
        int indexOf(String str) - returns the index of the first occurrence of str; returns -1 if not found
        int compareTo(String other)  -  returns a value < 0 if this is less than other 
                                        returns a value = 0 if this is equal to other 
                                        returns a value > 0 if this is greater than other
        
        Don't have to use, not given in the back of the card, but is REALLY helpful:
        char charAt(int index)
        */
        
        
        // length - the number of characters in the string
        // int length()  
        System.out.println("String length = " + str.length());
        
        // Addition of strings : Concatentation
        System.out.println(str + " -- " + str );

        // Whe using concatentation, one of them should always be a string
        // the easiest way to convert a value to a trsing is to add to an empty string.
        // "" + 90
        
        // substring
        // Sring Substring (from, pastEnd);
        // *not* including last index, in this case 7.
        System.out.println("substring :" + str.substring(0,7));
        
        // substring
        // String substring(int from) - returns substring(from, length())
        System.out.println("substring :" + str.substring(6));
        

        // String is composed of characters
        // char charAt(int index)
        // String chars start from 0!!
        System.out.println("3rd character is:" + str.charAt(3));

        
        char c1 = 'A' ;
        
        // Escape sequences
        String str2 = " quoting \" He said\" ";
        String str3 = " a \" string" ; 
        String str4 = " a new line\n " ; 
        
        char c2 = '\n';

        
        // indexOf  (char), or String
        // -1 if doesn't find
        System.out.println("index of H is" + str.indexOf('H'));


        
        // int compareTo(String other)  -  returns a value < 0 if this is less than other 
        //                                returns a value = 0 if this is equal to other 
        //                                returns a value > 0 if this is greater than other
        String str5 = "a string";
        String str6 = "b string";
        System.out.println(str5 + ".compareTO("+str6 + ")=" + str5.compareTo(str6));
        String str7 = "a string longer";
        System.out.println(str5 + ".compareTO("+str7 + ")=" + str5.compareTo(str7));
        String str8 = "A string";
        System.out.println(str5 + ".compareTO("+str8 + ")=" + str5.compareTo(str8));

        
        // CodingBat:
        //  go there, open an account, and then go to 
        // http://codingbat.com/pref
        // At the bottom, and zbaharav@kehillah.org  And then i can see your work!
        // Solve at-least 5 problems from there. 
        // char: single character + single quotes
        // One of the primitive types!

                
        // String compareTo   - Comparable
        // Object identity (==) .vs. object equality (equals)
        // Strings are immutable!
        String str9 = "a ";
        str9 = str9 +  "string";
        System.out.println(str5 + "==" + str9 + "  is " + (str5==str9));
        System.out.println(str5 + ".compareTo(" + str9 + ") is " + (str5.compareTo(str9)));
        System.out.println(str5 + ".equals(" + str9 + ") is " + (str5.equals(str9)));
        


        
        
        
        // OUR own class
        // Example for: Going over all elements of a String
        // String KHyphen()
        
        KString kstr;
        kstr = new KString("Hello");
        System.out.println(kstr +" hyphened is " + kstr.KHyphen());

        kstr = new KString("AB");
        System.out.println(kstr +" hyphened is " + kstr.KHyphen());
        
        kstr = new KString("A");
        System.out.println(kstr +" hyphened is " + kstr.KHyphen());
        
        kstr = new KString("");
        System.out.println(kstr +" hyphened is " + kstr.KHyphen2());
        
        


        // For the homework!!
        
        // KHyphen2()
        kstr = new KString("Hello");
        System.out.println(kstr +" hyphened2 is " + kstr.KHyphen2());

        kstr = new KString("ABC");
        System.out.println(kstr +" hyphened2 is " + kstr.KHyphen2());

        kstr = new KString("AB");
        System.out.println(kstr +" hyphened2 is " + kstr.KHyphen2());
        
        kstr = new KString("A");
        System.out.println(kstr +" hyphened2 is " + kstr.KHyphen2());
        
        kstr = new KString("");
        System.out.println(kstr +" hyphened2 is " + kstr.KHyphen2());
        
        // Palindrome
        
        kstr = new KString("AbA");
        System.out.println(kstr +"  is a palindrome:  " + kstr.isPalindrome());
        kstr = new KString("AbbA");
        System.out.println(kstr +"  is a palindrome:  " + kstr.isPalindrome());
        kstr = new KString("A");
        System.out.println(kstr +"  is a palindrome:  " + kstr.isPalindrome());
        kstr = new KString("Ab");
        System.out.println(kstr +"  is a palindrome:  " + kstr.isPalindrome());
        kstr = new KString("Abb");
        System.out.println(kstr +"  is a palindrome:  " + kstr.isPalindrome());
        kstr = new KString("");
        System.out.println(kstr +"  is a palindrome:  " + kstr.isPalindrome());

        
        
        
        
        
        
    }
    
}
