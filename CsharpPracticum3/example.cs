class Hello
{
    int g;
    
    void main()
    {
        //test// 
        //dit is een test
        bool a;
        //test// 
        //dit is een test
        a = true;
        //test// 
        //dit is een test   
        int b;
        b = 0;
        while(b < 10){
            b= b+ 1;            
        }
        b = 3333;
        int k;
        for(k = 0; k<10; k = k +1;){
            
        }
        b = 3333;
    }
    
    int square( int x )
    {
        int y;
        y = x*x;
        return y;   
    }

    int abs(int x)
    {
    	
        if (x<0)
            x = 0-x;
        return x;
    }
    
    int fac(int x)
    {
        int r; int t;
        t=1; r=1;
        while (t<=x)
        {
            r = r*t;
            t = t+1;
        }
       
        return r;
        
   }
}
