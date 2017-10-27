
public class KString {

    private String myString ;
    
    
    // constructor
    public KString() {
        myString = "";
    }
    
    public KString(String str) {
        myString = str;
    }
    
    // hyphen a string
    
    public String KHyphen() {
    String str="";
    
    if (myString.length() <=1 ) return myString;
    
    str = str + myString.charAt(0);

    for (int ii=1; ii<myString.length(); ii = ii+1) {
        str = str + '-' + myString.charAt(ii) ;
    }
    
    return str;    
        
    }
    
    public String toString() {
        return myString;
    }
    
    
    
    // DO NOT show: Hyphen every other one:
    // Write a function that returns a string where a hyphen is inserted every two
    // characters.
    // "ABCDE"  -->  "AB-CD-E"
    // Special cases
    // "AB"  -> "AB"
    // "A"   -> "A"
    // ""    -> ""
    
    public String KHyphen2() {
        String str="";

        if (myString.length() <=2 ) return myString;

        str = str + myString.substring(0,2);

        for (int ii=2; ii<myString.length(); ii = ii+1) {
            if (ii%2 == 0)
                str += "-";
            str = str + myString.charAt(ii) ;
        }

        return str;    

    }


    public boolean isPalindrome()
    {
        boolean pali = true ;
        
        /*
        for (int ii=0; ii<myString.length() ; ++ii) {
            if (myString.charAt(ii) != myString.charAt(myString.length()-1-ii))
                pali = false;
        }
        */
        
        // Two ways to improve:
        // 1. Search only to half way
        // 2. break when found bad char
        
        for (int ii=0; ii<myString.length()/2 && pali ; ++ii) {
            if (myString.charAt(ii) != myString.charAt(myString.length()-1-ii))
                pali = false;
        }
        
        
        
        return pali;
    }
    
    
    
    
}
