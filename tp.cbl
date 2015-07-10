       IDENTIFICATION DIVISION.
       PROGRAM-ID. TP.
     
       ENVIRONMENT DIVISION.
	   INPUT-OUTPUT SECTION.
	   FILE-CONTROL.
	    SELECT NOV-TIMES1	ASSIGN TO DISK
							ORGANIZATION IS LINE SEQUENTIAL
							FILE STATUS IS NOV-TIMES1-ESTADO.
	    SELECT NOV-TIMES2	ASSIGN TO DISK
	  						ORGANIZATION IS LINE SEQUENTIAL
	  						FILE STATUS IS NOV-TIMES2-ESTADO.
	    SELECT NOV-TIMES3	ASSIGN TO DISK
	  						ORGANIZATION IS LINE SEQUENTIAL
	  						FILE STATUS IS NOV-TIMES3-ESTADO.
		SELECT CONSULTORES	ASSIGN TO DISK
	  						ORGANIZATION IS LINE SEQUENTIAL
	  						FILE STATUS IS CONSULTORES-ESTADO.
		SELECT TIM		ASSIGN TO DISK
	  					ORGANIZATION IS LINE SEQUENTIAL
	  					FILE STATUS IS TIMES-ESTADO.
		SELECT EMPRESAS	ASSIGN TO DISK
	  					ORGANIZATION IS LINE SEQUENTIAL
	  					FILE STATUS IS EMPRESAS-ESTADO.
		SELECT CATEGORIAS	ASSIGN TO DISK
	  						ORGANIZATION IS LINE SEQUENTIAL
	  						FILE STATUS IS CATEGORIAS-ESTADO.
		SELECT LISTADOA	ASSIGN TO PRINTER.
		SELECT LISTADOB	ASSIGN TO PRINTER.
		
	   DATA DIVISION.
	   FILE SECTION.
	   
	   FD NOV-TIMES1 	LABEL RECORD IS STANDARD
						VALUE OF FILE-ID IS "NOV1.DAT".
	   01 REG-NOV-TIMES1.
			03 NOV-TIMES1-NUMERO	PIC X(5).
			03 NOV-TIMES1-FECHA.
				05 NOV-TIMES1-ANIO	PIC 9(4).
				05 NOV-TIMES1-MES	PIC 9(2).
				05 NOV-TIMES1-DIA	PIC 9(2).
			03 NOV-TIMES1-EMPRESA	PIC 9(3).
			03 NOV-TIMES1-TAREA		PIC X(4).
			03 NOV-TIMES1-HORAS		PIC 9(2)V99.
			
	   FD NOV-TIMES2 	LABEL RECORD IS STANDARD
						VALUE OF FILE-ID IS "NOV2.DAT".
	   01 REG-NOV-TIMES2.
		03 NOV-TIMES2-NUMERO	PIC X(5).
	  	03 NOV-TIMES2-FECHA.
				05 NOV-TIMES2-ANIO	PIC 9(4).
				05 NOV-TIMES2-MES	PIC 9(2).
				05 NOV-TIMES2-DIA	PIC 9(2).
	  	03 NOV-TIMES2-EMPRESA	PIC 9(3).
	  	03 NOV-TIMES2-TAREA		PIC X(4).
	  	03 NOV-TIMES2-HORAS		PIC 9(2)V99.
	  	
	   FD NOV-TIMES3 	LABEL RECORD IS STANDARD
	  				VALUE OF FILE-ID IS "NOV3.DAT".
	   01 REG-NOV-TIMES3.
	  	03 NOV-TIMES3-NUMERO	PIC X(5).
	  	03 NOV-TIMES3-FECHA.
				05 NOV-TIMES3-ANIO	PIC 9(4).
				05 NOV-TIMES3-MES	PIC 9(2).
				05 NOV-TIMES3-DIA	PIC 9(2).
	  	03 NOV-TIMES3-EMPRESA	PIC 9(3).
	  	03 NOV-TIMES3-TAREA		PIC X(4).
	  	03 NOV-TIMES3-HORAS		PIC 9(2)V99.
		
	   FD CONSULTORES 	LABEL RECORD IS STANDARD
	  				VALUE OF FILE-ID IS "CONSULTORES.DAT".
	   01 REG-CONS.
	  	03 CONS-NUMERO	PIC X(5).
	  	03 CONS-DNI		PIC 9(8).
	  	03 CONS-SRT		PIC X(2).
	  	03 CONS-NOMBRE	PIC X(25).
	  	03 CONS-DIRE	PIC X(20).
	  	03 CONS-TEL		PIC X(20).
		
	   FD TIM 	LABEL RECORD IS STANDARD
				VALUE OF FILE-ID IS "TIMES.DAT".
	   01 REG-TIM.
	  	03 TIM-NUMERO	PIC X(5).
	  	03 TIM-FECHA		PIC 9(8).
	  	03 TIM-EMPRESA		PIC X(3).
	  	03 TIM-TAREA	PIC X(4).
	  	03 TIM-HORAS	PIC 9(2)V99.
	
	   FD EMPRESAS 	LABEL RECORD IS STANDARD
	  				VALUE OF FILE-ID IS "EMPRESAS.DAT".
	   01 REG-EMPRESAS.
	  	03 EMP-EMPRESA	PIC X(3).
	  	03 EMP-RAZON		PIC X(25).
	  	03 EMP-DIRE		PIC X(20).
	  	03 EMP-TEL		PIC X(20).
	  	03 EMP-CUIT	PIC 9(11).
		
	   FD CATEGORIAS 	LABEL RECORD IS STANDARD
	  				VALUE OF FILE-ID IS "CATEGORIAS.DAT".
	   01 REG-CATEGORIAS.
	  	03 CAT-SRT		PIC X(2).
	  	03 CAT-DESC		PIC X(20).
	  	03 EMP-TARIFA	PIC 9(5)V99.
	  	
	   FD LISTADOA 	LABEL RECORD OMITTED.
	   01 LINEAA	PIC X(80).
	  
	   FD LISTADOB 	LABEL RECORD OMITTED.
	   01 LINEAB	PIC X(80).
		
	   
	   WORKING-STORAGE SECTION. 
	   77 NOV-TIMES1-ESTADO PIC XX.
	   77 NOV-TIMES2-ESTADO PIC XX.
	   77 NOV-TIMES3-ESTADO PIC XX.
	   77 CONSULTORES-ESTADO PIC XX.
	   77 TIMES-ESTADO PIC XX.
	   77 EMPRESAS-ESTADO PIC XX.
	   77 CATEGORIAS-ESTADO PIC XX.
	   77 EOF-NOVTIMES1 PIC XX VALUE "NO".
			88 EOF-NOV-TIMES1 VALUE "SI".
	   77 EOF-NOVTIMES2 PIC XX VALUE "NO".
			88 EOF-NOV-TIMES2 VALUE "SI".
	   77 EOF-NOVTIMES3 PIC XX VALUE "NO".
			88 EOF-NOV-TIMES3 VALUE "SI".
	   77 EOF-CONS PIC XX VALUE "NO".
			88 EOF-CONSULTORES VALUE "SI".
	   77 EOF-TIM PIC XX VALUE "NO".
			88 EOF-TIMES VALUE "SI".
	   77 EOF-EMP PIC XX VALUE "NO".
			88 EOF-EMPRESAS VALUE "SI".
	   77 EOF-CAT PIC XX VALUE "NO".
			88 EOF-CATEGORIAS VALUE "SI".
	   
	   01 CLAVE-NOV-TIMES1.
			03 CLAVE-NOV-TIMES1-NUMERO 	PIC 9(5).
			03 CLAVE-NOV-TIMES1-FECHA	PIC 9(8).
	   01 CLAVE-NOV-TIMES2.
			03 CLAVE-NOV-TIMES2-NUMERO 	PIC 9(5).
			03 CLAVE-NOV-TIMES2-FECHA	PIC 9(8).
	   01 CLAVE-NOV-TIMES3.
			03 CLAVE-NOV-TIMES3-NUMERO 	PIC 9(5).
			03 CLAVE-NOV-TIMES3-FECHA	PIC 9(8).
		
     
       PROCEDURE DIVISION.
       COMIENZO.
			DISPLAY 'Hola mundo'.
			PERFORM INICIO.
			PERFORM LEER-NOV-TIMES1.
			PERFORM LEER-NOV-TIMES2.
			PERFORM LEER-NOV-TIMES3.
			PERFORM LEER-CONSULTORES.
			PERFORM PROCESAR-ARCHIVOS UNTIL EOF-NOV-TIMES1 
			AND EOF-NOV-TIMES2 AND EOF-NOV-TIMES3.
			PERFORM CERRAR-NOVEDADES.
			STOP RUN.
		
	   INICIO.
	    OPEN INPUT NOV-TIMES1.
	    OPEN INPUT NOV-TIMES2.
	    OPEN INPUT NOV-TIMES3.
	    OPEN INPUT CONSULTORES.
	    OPEN INPUT TIM.
	    OPEN INPUT EMPRESAS.
	    OPEN INPUT CATEGORIAS.
	    OPEN OUTPUT LISTADOA.
	    OPEN OUTPUT LISTADOB.
	
	   CERRAR-NOVEDADES.
	    CLOSE NOV-TIMES1.
	    CLOSE NOV-TIMES2.
	    CLOSE NOV-TIMES3.
	    CLOSE CONSULTORES.
	    CLOSE TIM.
	    CLOSE EMPRESAS.
	    CLOSE CATEGORIAS.
		
	   LEER-NOV-TIMES1.
	    READ NOV-TIMES1 
			AT END MOVE "SI" TO EOF-NOVTIMES1.
		MOVE NOV-TIMES1-FECHA TO CLAVE-NOV-TIMES1-FECHA.
		MOVE NOV-TIMES1-NUMERO TO CLAVE-NOV-TIMES1-NUMERO.
	   
	   LEER-NOV-TIMES2.
	    READ NOV-TIMES2 
			AT END MOVE "SI" TO EOF-NOVTIMES2.
		MOVE NOV-TIMES1-NUMERO TO CLAVE-NOV-TIMES2-NUMERO.
		MOVE NOV-TIMES1-FECHA TO CLAVE-NOV-TIMES2-FECHA.
	   
	   LEER-NOV-TIMES3.
	    READ NOV-TIMES3 
			AT END MOVE "SI" TO EOF-NOVTIMES3.
		MOVE NOV-TIMES1-NUMERO TO CLAVE-NOV-TIMES3-NUMERO.
		MOVE NOV-TIMES1-FECHA TO CLAVE-NOV-TIMES3-FECHA.
	   
	   LEER-CONSULTORES.
	    READ CONSULTORES
			AT END MOVE "SI" TO EOF-CONS.
		
	   PROCESAR-ARCHIVOS.
	    PERFORM LEER-NOV-TIMES1.
		PERFORM LEER-NOV-TIMES2.
		PERFORM LEER-NOV-TIMES3.
	
		
		