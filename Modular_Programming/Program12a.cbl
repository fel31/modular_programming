       environment division .
       file-control .
           SELECT EMPLOYEE-FILE 
               ASSIGN "\COBOLClass_Eclipse\DataFiles\EMPLOYEE12.DAT"
               ORGANIZATION IS INDEXED
               ACCESS IS RANDOM
               RECORD KEY EMPLOYEE-NO
               file status ws-status .
       data division .
       FD  EMPLOYEE-FILE .
       01  EMPLOYEE-REC .
           03  EMPLOYEE-NO      PIC X(8) .
           03  EMPLOYEE-INITS   PIC X(4) .
           03  EMPLOYEE-SURNAME PIC X(16) .
           03  EMPLOYEE-SALARY  PIC 9(6)V99 .
           03  EMPLOYEE-ADDRESS PIC X(40) .
           03  EMPLOYEE-DEPT    PIC X(10) .

       working-storage section .

       01  ws-status            pic xx .

       linkage section .
       01  ls-EMPLOYEE-REC .
           03  ls-EMPLOYEE-NO      PIC X(8) .
           03  ls-EMPLOYEE-INITS   PIC X(4) .
           03  ls-EMPLOYEE-SURNAME PIC X(16) .
           03  ls-EMPLOYEE-SALARY  PIC 9(6)V99 .
           03  ls-EMPLOYEE-ADDRESS PIC X(40) .
           03  ls-EMPLOYEE-DEPT    PIC X(10) .
       01  ls-action            pic x .
       01  ls-status            pic xx .

       procedure division using ls-employee-rec
                                ls-action
                                ls-status .
       init-para .
           open i-o employee-file .
           if ws-status unequal zero
               move ws-status to ls-status
               move all "!" to ls-employee-rec
               exit program
           end-if
           evaluate ls-action
             when "R"
               perform read-file
             when "A"
               perform add-record
             when "C"
               perform change-record
             when "D"
               perform delete-record
           end-evaluate
           move ws-status to ls-status .
           close employee-file .
           exit program .
       read-file .
           move ls-employee-no to employee-no .
           read employee-file .
           move employee-rec to ls-employee-rec .
       add-record .
           move ls-employee-rec to employee-rec .
           write employee-rec .
       change-record .
           IF ls-EMPloyee-SALARY UNEQUAL ZERO
               MOVE ls-EMPloyee-SALARY TO EMPLOYEE-SALARY .
           IF ls-EMPloyee-ADDRESS UNEQUAL SPACES
               MOVE ls-EMPloyee-ADDRESS TO EMPLOYEE-ADDRESS .
           IF ls-EMPloyee-DEPT UNEQUAL SPACES
               MOVE ls-EMPloyee-DEPT TO EMPLOYEE-DEPT .
           rewrite employee-rec .
       delete-record .
           delete employee-file .
