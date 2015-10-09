public class Prueba{

public static double evGlotona(){
		int a =3;
		int b = 4;
	double c = Math.sqrt(a + b);
	return c;
}	

public static int f( int a ){
    return 5;
}
public static int g( int a ){
    return a - 8;
}

public static void main(String[] args){
	int ge= g(8);
	int func = f( 5 / ge);
	System.out.println(func);
}

}
