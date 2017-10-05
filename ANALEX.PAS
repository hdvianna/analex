    Program AnaLex;

        {Lista dinƒmica que guarda posi‡oes onde existem carcteres em branco}
        Uses
            Crt;
        Const
             MAXRANGE = 58;

             MAXRESERVADAS = 58;
             MAXDELIMITADORES = 10;
             MAXOPERADORES = 9;

             ENTRADA = 'c:\analisar.lex';
             SAIDA = 'c:\fim.txt';
             PATHRESERVADAS = 'c:\reservad.txt';
             PATHOPERADORES = 'c:\operador.txt';
             PATHDELIMITADORES = 'c:\delimita.txt';
        Type

            str255 = String[255];
            str32 = String[32];
            MyArray = Array [1..MAXRANGE] of str32;
            txtFile = Text;

            ptrLista= ^spcChars;
            spcChars = Record {Lista Encadeada com os lugares onde h  espa‡os em branco}
                  Pos:Integer;
                  prox:ptrLista;
            End;

            ptrAtom = ^Atoms;
            Atoms = Record
                  Atom:str32;
                  Count:Integer;
                  prox:ptrAtom;
            End;


        Function UCase(strWord:str255):str255;
        Var
           i:integer;
           strAux:str255;
        Begin
             strAux:='';
             For i:=1 to Length(strWord) do
                  strAux:=strAux + UpCase(strWord[i]);
             UCase:=strAux;
        end;

        Function RetiraPalavra(strCheck:str255;strMatch:str32;Pos:Integer):str32;
        Var
           strAux:str255;
        Begin
             If Pos > 1 then
                strAux:=Copy(strCheck,1,Pos-1)
             else
                 strAux:='';

             If Pos < length(strCheck) then
                strAux:=strAux+' '+Copy(strCheck,(Pos+Length(strMatch)),Length(strCheck));
             RetiraPalavra:=strAux;
        end;
        Procedure AbreArquivo(Caminho:str255;Var mFile:txtFile);
        Begin

             Assign(mFile,Caminho);
             Append(mFile);
             Reset(mFile);

        End;
        Procedure CriaArquivo (Caminho:str255;Var mFile:txtFile);
        Begin
              Assign(mFile,Caminho);
              Rewrite(mFile);
        End;

        Procedure AddAtom(Var lexAtoms:ptrAtom;strAtom:str32);
        Var
           ptrAux,lexAtom:ptrAtom;
           Achou:Boolean;
        Begin

             ptrAux := lexAtoms;

             If ptrAux = Nil then
             Begin
                New(lexAtoms);
                lexAtoms^.Count:=1;
                lexAtoms^.Atom:=strAtom;
                lexAtoms^.prox:=Nil;
             End
             Else
             Begin
                 Achou:=False;

                 While (Not Achou) And (ptrAux^.prox <> nil) Do
                 Begin
                    If ptrAux^.Atom = strAtom Then
                    Begin
                        ptrAux^.count:=ptrAux^.count+1;
                        Achou:=True;
                    end;
                    ptrAux:=ptrAux^.prox;
                 End;

                 If Not Achou then
                 Begin
                     If ptrAux^.Atom = strAtom then
                         ptrAux^.count:=ptrAux^.count+1
                     Else Begin
                          New(lexAtom);
                          lexAtom^.Atom:=strAtom;
                          lexAtom^.count:=1;
                          lexAtom^.prox:=nil;
                          ptrAux^.prox:=lexAtom;
                     end;
                 End;
             End;

        End;

        Procedure mkArray(Var arqArray:txtFile;Var Vetor:MyArray);
        Var
            i:Integer;
            strItem:str32;
        Begin
            i:=1;
            While Not (EOF(arqArray)) Do
            Begin
                 Readln(arqArray,strItem);
                 Vetor[i]:=strItem;
                 i:=i+1;
            End;

        End;

        Function FindWord(Vetor:MyArray;strWord:str32;MAX:Integer):Boolean;
        Var
           i    : Integer;
           Achou:Boolean;
        Begin
             i:=1;
             Achou:=False;
             While (Not(Achou) And (i <= MAX)) Do
             Begin
                  If Vetor[i] = strWord Then
                  Begin
                     Achou:=True;
                  End;
                  i:=i+1;
             End;
             FindWord:=Achou;
        End;

        Procedure GetUltimoPtr(Ponteiro:ptrLista;Var Ultimo:ptrLista);
        Var
           ptrAuxiliar:ptrLista;
        Begin

             ptrAuxiliar:=Ponteiro;
             While (ptrAuxiliar^.prox <> Nil) Do
             Begin
                  ptrAuxiliar:=ptrAuxiliar^.prox;
             End;

             Ultimo:=ptrAuxiliar;

        End;

        Procedure AchaPosInsOrd(Var ptrPri,ptrAnt,ptrProx:ptrLista;Valor:Integer);
        Var
           ptrAux:ptrLista;
           Fim:Boolean;
        Begin

             ptrAux:=ptrPri;
             Fim:=False;
             ptrAnt:=Nil;

             If (ptrAux = Nil) Then
                ptrProx:=Nil

             Else begin
                If (ptrAux^.prox = Nil) Then
                Begin
                  If  (ptrAux^.Pos >= Valor) then
                  begin
                      ptrProx:=ptrAux;
                      ptrAnt:=ptrPri;
                  end
                  else begin

                  end;
                End
                Else begin
                   While (Not Fim) And (ptrAux^.prox <> Nil) Do
                   Begin

                        If (ptrAux^.Pos >= Valor) Then
                        Begin
                             Fim:=True;
                             ptrAnt:=ptrAux;
                             ptrProx:=ptrAux^.prox;
                        end
                        Else begin
                             ptrAux:=ptrAux^.prox
                        End;

                   End;

                end;

             End;

        End;

        Procedure AddInLista(Ponteiro:ptrLista;Posicao:Integer);{Adiciona posi‡ao na lista}
        Var
             Elemento,Ultimo:ptrLista;
        Begin

             GetUltimoPtr(Ponteiro,Ultimo);
             new(Elemento);
             Elemento^.Pos:=Posicao;
             Elemento^.prox:=Nil;
             Ultimo^.prox:=Elemento;

        End;

        Procedure AddInListaOrd(Var ptrPrimeiro:ptrLista;Pos:Integer);
        Var
           ptrAux,ptrCursor,ptrAnt,ptrProx:ptrLista;
           Inseriu:Boolean;
        Begin

             If ptrPrimeiro = Nil then
             begin
                 New(ptrPrimeiro);
                 ptrPrimeiro^.pos:=Pos;
                 ptrPrimeiro^.prox:= Nil;
             end
             else begin

                  New(ptrAux);
                  ptrAux^.pos:=Pos;

                  ptrCursor:=ptrPrimeiro;
                  Inseriu:=False;
                  ptrAnt:=nil;
                  While (ptrCursor^.prox <> Nil) And Not Inseriu do
                  begin
                       If ptrCursor^.Pos > ptrAux^.Pos Then
                       Begin
                          ptrAux^.prox:=ptrCursor;
                          ptrAnt^.prox:=ptrAux;
                          Inseriu:=True;
                       end;
                       ptrAnt:=ptrCursor;
                       ptrCursor:=ptrCursor^.prox;
                  end;

                  If Not Inseriu then
                  begin
                     If ptrCursor^.pos > Pos then
                     begin
                        ptrAnt^.prox:=ptrAux;
                        ptrAux^.prox:=ptrCursor;
                        ptrPrimeiro:=ptrAux;
                     end
                     else begin
                        ptrCursor^.prox:=ptrAux;
                        ptrAux^.prox:=nil;
                     end;

                  end;
             end;

        End;

        Function Instr(strCheck:str255;strMatch:str32;Inicio:Integer):Integer;
        Var
             strAux     :str255;
             i,Pos,Tam  :Integer;
             Achou      :Boolean;
        Begin

             i:=Inicio;
             Tam:=Length(strMatch);
             Achou:=False;
             While (i <= Length(strCheck)) And (Not(Achou)) Do
             Begin
                  strAux:=Copy(strCheck,i,Tam);
                  If strAux = strMatch Then
                  Begin
                     Pos:=i;
                     Achou:=True;
                  End;
                  Inc(i);
             End;

             If Achou Then
                Instr:=Pos
             Else
                Instr:=0;

        End;


        Procedure BuscaChars (Var Ponteiro:ptrLista;strMatch:str255;strCheck:str32);
        Var
            i,Pos,Tam:Integer;
        Begin

             Pos:=1;
             Tam:=Length(strMatch);
             While (Pos > 0) Do
             Begin
                  Pos:=Instr(strCheck,strMatch,Pos);
                  If Pos > 0  Then
                  Begin
                       AddInListaOrd(Ponteiro,Pos);
                       Pos := Pos + 1;
                  End;
             End;
        End;

        Procedure AnalizeWords(Var strCheck:str255;Var lexAtoms:ptrAtom;MAXSUB:integer;Vetor:MyArray);
        Var
           i,Pos:integer;
        Begin
            For i:=1 to MAXSUB do
            Begin
                Pos:=1;
                While (Pos > 0) Do
                Begin
                    Pos:=Instr(UCase(strCheck),Vetor[i],Pos);
                    If Pos > 0  Then
                    Begin
                       AddAtom(lexAtoms,Vetor[i]);
                       strCheck:=RetiraPalavra(strCheck,Vetor[i],Pos);
                       Pos := Pos + 1;
                    End;
                End;
             end;

        end;
        Function RetiraComentarios(ptrAComent,ptrFComent:ptrLista;strLinha:str255):str255;
        Var
           strAux:str32;
           Pos,intIni,intFim,i:Integer;
        Begin

             GetUltimoPtr(ptrFComent,ptrFComent);
             If ptrAComent^.Pos > 1 then
                strAux:=Copy(strLinha,1,(ptrAComent^.Pos)-1)
             else
                 strAux:='';

             If ptrFComent^.Pos < length(strLinha) then
                strAux:=strAux+Copy(strLinha,(ptrFcoment^.Pos)+1,Length(strLinha));

             RetiraComentarios:=strAux;

        End;

        Procedure WriteInFile(ptrAtoms:ptrAtom;strLabel:str32;var EndFile:txtFile);
        Var
           ptrAux:ptrAtom;
        Begin
             ptrAux:=ptrAtoms;
             Writeln(endFile,strLabel);
             While ptrAux <> Nil do
             Begin
                  Writeln(endFile,chr(9),ptrAux^.Atom);
                  Writeln(endFile,chr(9),chr(9),ptrAux^.count,' encontrados');
                  ptrAux:=ptrAux^.prox;
             end;
        end;

        Function RetiraEspacosDuplos(strVictim:str32):str255;
        Var
           Pos:integer;
        Begin
             While Pos > 0 do
             Begin
                  Pos:=Instr(strVictim,'  ',1);
                  strVictim:=RetiraPalavra(strVictim,'  ',Pos);
             end;
             RetiraEspacosDuplos:=strVictim;
        end;
        Function Trim(strLinha:str255):str255;
        Var
          i,j:Integer;
        Begin
             i:=1;
             j:=Length(strLinha);
             While strLinha[i] = ' ' Do
                inc(i);

             While strLinha[j] = ' ' Do
                dec(j);

             strLinha:=copy(strLinha,i,(j-i)+1);
             Trim:=strLinha;
        End;

        Procedure getIdentificadores(Var ptrIdentificadores:ptrAtom;strLinha:str255);
        Var
           strAux:str32;
           Pos,Posii:Integer;
        Begin

                  strLinha:=Trim(RetiraEspacosDuplos(strLinha));
                  Pos:=Instr(strLinha,' ',1);
                  While Pos > 0 Do
                  begin

                    Pos :=Instr(strLinha,' ',1);
                    If Pos > 0 then
                    Begin
                         strAux:=Trim(Copy(strLinha,1,Pos));
                         strLinha:=RetiraPalavra(strLinha,strAux,1);
                         strLinha:=Trim(strLinha);
                         AddAtom(ptrIdentificadores,strAux);
                    end
                    else
                         AddAtom(ptrIdentificadores,strLinha);

                  end;

        End;

        Procedure AnalisaLinha(strLinha:str255;
                              ptrBranco,ptrAComent,ptrFComent:ptrLista;
                              Var EndFile:txtFile;
                              Var ptrReservadas:ptrAtom;
                              Var ptrDelimitadores:ptrAtom;
                              Var ptrOperadores:ptrAtom;
                              Var ptrIdentificadores:ptrAtom);
        Var
            aReservadas,aDelimitadores,aOperadores:MyArray;
            fReservadas,fOperadores,fDelimitadores:txtFile;
        Begin

             AbreArquivo(PATHRESERVADAS,fReservadas);
             mkArray(fReservadas,aReservadas);

             AbreArquivo(PATHOPERADORES,fOperadores);
             mkArray(fOperadores,aOperadores);

             AbreArquivo(PATHDELIMITADORES,fDelimitadores);
             mkArray(fDelimitadores,aDelimitadores);

             BuscaChars(ptrBranco,' ',strLinha);
             BuscaChars(ptrAComent,'(*',strLinha);
             BuscaChars(ptrAComent,'{',strLinha);
             BuscaChars(ptrFComent,'}{',strLinha);
             BuscaChars(ptrFComent,'*)',strLinha);
             strLinha:=RetiraComentarios(ptrAComent,ptrFComent,strLinha);

             AnalizeWords(strLinha,ptrReservadas,MAXRESERVADAS,aReservadas);
             AnalizeWords(strLinha,ptrOperadores,MAXOPERADORES,aOperadores);
             AnalizeWords(strLinha,ptrDelimitadores,MAXDELIMITADORES,aDelimitadores);

             strLinha:=RetiraEspacosDuplos(strLinha);
             getIdentificadores(ptrIdentificadores,strLinha);

             Close(fReservadas);
             Close(fDelimitadores);
             Close(fOperadores);

        End;

        Procedure MontaSaida(Var LexFile,EndFile:txtFile);
        Var
            strLinha:str255;
            ptrBranco,ptrAComent,ptrFComent:ptrLista;
            ptrDelimitadores,ptrOperadores,ptrReservadas,ptrIdentificadores:ptrAtom;

        Begin
             ptrDelimitadores:=Nil;
             ptrOperadores:=Nil;
             ptrReservadas:=Nil;
             ptrIdentificadores:=Nil;

             While Not(EOF(LexFile)) Do
             Begin
                  ptrBranco:=Nil;
                  ptrAComent:=Nil;
                  ptrFComent:=Nil;

                  Readln(LexFile,strLinha);
                  AnalisaLinha(strLinha,ptrBranco,
                     ptrAComent,ptrFComent,EndFile,
                     ptrReservadas,
                     ptrDelimitadores,
                     ptrOperadores,
                     ptrIdentificadores);
             End;

             WriteInFile(ptrReservadas,'Palavras Reservadas',endFile);
             WriteInFile(ptrOperadores,'Operadores',endFile);
             WriteInFile(ptrDelimitadores,'Delimitadores',EndFile);
             WriteInFile(ptrIdentificadores,'Identificadores',EndFile);

             close(lexFile);
             close(EndFile);
        End;


    Var
       inFile,outFile:txtFile;

    Begin

         clrscr;

         AbreArquivo(ENTRADA,inFile);

         CriaArquivo(SAIDA,outFile);

         MontaSaida(inFile,outFile);

         Writeln('Acab“');

         readln;

    End.