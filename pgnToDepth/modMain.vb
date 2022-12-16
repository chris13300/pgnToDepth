Module modMain

    Sub Main()
        Dim lecture As IO.StreamReader, ligne As String, fichierPGN As String
        Dim chaine As String, totProf(1000) As Integer, nbProf(1000) As Integer, nbParties(1000) As Integer, totDuree(1000) As Single
        Dim partie As String, joueurBlanc As String, joueurNoir As String
        Dim tabPartie() As String, coupBlanc As Boolean, tabJoueurs(1000) As String
        Dim indexBlanc As Integer, indexNoir As Integer, nbCoups As Integer, totCoups As Integer
        Dim maxLongueur As Integer, pgn_court As String
        Dim taille As Long, cumul As Long, i As Integer

        If My.Computer.FileSystem.GetFileInfo(Environment.GetFolderPath(Environment.SpecialFolder.UserProfile) & "Documents\Visual Studio 2013\Projects\pgnToDepth\pgnToDepth\bin\Debug\pgnToDepth.exe").LastWriteTime > My.Computer.FileSystem.GetFileInfo(My.Application.Info.AssemblyName & ".exe").LastWriteTime Then
            MsgBox("Il existe une version plus récente de ce programme !", MsgBoxStyle.Information)
            End
        End If

        fichierPGN = Replace(Command(), """", "")
        If Not My.Computer.FileSystem.FileExists(fichierPGN) Then
            Console.WriteLine("pgnToDepth.exe ""path_to_file.pgn""")
            End
        End If
        taille = FileLen(fichierPGN)
        pgn_court = nomFichier(fichierPGN)

        indexBlanc = -1
        indexNoir = -1
        tabJoueurs(0) = "0"
        maxLongueur = 0
        cumul = 0
        totCoups = 0
        lecture = New IO.StreamReader(fichierPGN)
        Do
            'on récupère le joueurBlanc, le joueurNoir et la partie en une seule ligne
            joueurBlanc = ""
            joueurNoir = ""
            partie = ""
            nbCoups = 0
            Do
                ligne = lecture.ReadLine & " " 'ajout espace pour éviter prof non suivi d'un espace
                cumul = cumul + Len(ligne) + 1 'normalement il faut ajouter +2 pour le vbCrLf mais là on a déjà ajouté un espace
                If InStr(ligne, "[White ") > 0 Then
                    joueurBlanc = Trim(LCase(Replace(Replace(Replace(ligne, "[White ", ""), """", ""), "]", "")))

                    If Len(joueurBlanc) > maxLongueur Then
                        maxLongueur = Len(joueurBlanc)
                    End If

                    For indexBlanc = 1 To CInt(tabJoueurs(0))
                        If tabJoueurs(indexBlanc) = joueurBlanc Then
                            Exit For
                        End If
                    Next

                    If indexBlanc > CInt(tabJoueurs(0)) Then
                        tabJoueurs(indexBlanc) = joueurBlanc
                        tabJoueurs(0) = Format(CInt(tabJoueurs(0)) + 1)
                    End If

                ElseIf InStr(ligne, "[Black ") > 0 Then
                    joueurNoir = Trim(LCase(Replace(Replace(Replace(ligne, "[Black ", ""), """", ""), "]", "")))

                    If Len(joueurNoir) > maxLongueur Then
                        maxLongueur = Len(joueurNoir)
                    End If

                    For indexNoir = 1 To CInt(tabJoueurs(0))
                        If tabJoueurs(indexNoir) = joueurNoir Then
                            Exit For
                        End If
                    Next

                    If indexNoir > CInt(tabJoueurs(0)) Then
                        tabJoueurs(indexNoir) = joueurNoir
                        tabJoueurs(0) = Format(CInt(tabJoueurs(0)) + 1)
                    End If

                ElseIf ligne <> " " And InStr(ligne, "[") = 0 And InStr(ligne, "]") = 0 Then
                    partie = partie & ligne & " "

                End If
            Loop Until lecture.EndOfStream _
                    Or InStr(partie, "1/2-1/2", CompareMethod.Text) > 0 _
                    Or InStr(partie, "1-0", CompareMethod.Text) > 0 _
                    Or InStr(partie, "0-1", CompareMethod.Text) > 0 _
                    Or InStr(partie, "*", CompareMethod.Text) > 0

            If partie = "" Then
                Exit Do
            End If

            'on formate la partie
            While InStr(partie, "  ") > 0 Or InStr(partie, "...") > 0 Or InStr(partie, "{ ") > 0
                partie = Replace(partie, "  ", " ")
                partie = Replace(partie, "...", " ")
                partie = Replace(partie, "{ ", "{")
            End While

            If InStr(partie, "s, ", CompareMethod.Text) > 0 Then
                partie = partie.Substring(0, partie.IndexOf("s, ") + 1) & "}"
            End If

            'on récupère la profondeur pour chaque joueur
            tabPartie = Split(partie, " ")
            coupBlanc = False
            For i = 0 To UBound(tabPartie)
                'changement de couleur
                If Len(tabPartie(i)) >= 2 And InStr(tabPartie(i), ".") = 0 And InStr(tabPartie(i), "{") = 0 And InStr(tabPartie(i), "/") = 0 And InStr(tabPartie(i), "}") = 0 Then
                    coupBlanc = Not coupBlanc
                    nbCoups = nbCoups + 1
                End If
                If InStr(tabPartie(i), "pat", CompareMethod.Text) = 0 Then
                    If InStr(tabPartie(i), "{") > 0 And InStr(tabPartie(i), ".") > 0 And InStr(tabPartie(i), "/") > 0 Then
                        'on récupère la profondeur
                        chaine = tabPartie(i).Substring(tabPartie(i).IndexOf("/") + 1)
                        If InStr(chaine, "}") Then
                            chaine = chaine.Substring(0, chaine.IndexOf("}"))
                        End If

                        'on attribue la profondeur au bon joueur
                        If IsNumeric(chaine) Then
                            If coupBlanc Then
                                totProf(indexBlanc) = totProf(indexBlanc) + CInt(chaine)
                                nbProf(indexBlanc) = nbProf(indexBlanc) + 1
                            Else
                                totProf(indexNoir) = totProf(indexNoir) + CInt(chaine)
                                nbProf(indexNoir) = nbProf(indexNoir) + 1
                            End If

                            'statistiques générales
                            totProf(0) = totProf(0) + CInt(chaine)
                            nbProf(0) = nbProf(0) + 1
                        End If

                    ElseIf InStr(tabPartie(i), "}") > 0 Then
                        If 1 < Len(tabPartie(i)) Then
                            If InStr(tabPartie(i - 1), "{") > 0 And InStr(tabPartie(i - 1), ".") > 0 And InStr(tabPartie(i - 1), "/") > 0 Then
                                'on récupère la durée
                                chaine = Replace(Replace(Replace(tabPartie(i), "}", ""), "s", ""), ".", ",")

                                'on attribue la profondeur au bon joueur
                                If IsNumeric(chaine) Then
                                    If coupBlanc Then
                                        totDuree(indexBlanc) = totDuree(indexBlanc) + CSng(chaine)
                                    Else
                                        totDuree(indexNoir) = totDuree(indexNoir) + CSng(chaine)
                                    End If

                                    'statistiques générales
                                    totDuree(0) = totDuree(0) + CSng(chaine)
                                Else
                                    MsgBox("tabPartie(i-1) = " & tabPartie(i - 1) & vbCrLf _
                                         & "tabPartie(i+0) = " & tabPartie(i) & vbCrLf _
                                         & "tabPartie(i+1) = " & tabPartie(i + 1), MsgBoxStyle.Critical)
                                    End
                                End If
                            End If
                        End If
                    End If
                End If
            Next

            nbParties(0) = nbParties(0) + 1
            nbParties(indexBlanc) = nbParties(indexBlanc) + 1
            nbParties(indexNoir) = nbParties(indexNoir) + 1
            totCoups = totCoups + nbCoups

            If nbParties(0) Mod 50 = 0 Then
                Console.Clear()
                Console.Title = My.Computer.Name & " : " & pgn_court & " @ " & Format(cumul / taille, "0.00%")
                chaine = Format(CInt(tabJoueurs(0)), "000 players")
                Console.WriteLine(chaine & " : avg. D" & Format(totProf(0) / nbProf(0), "00") & ", " & Format(1000 * totDuree(0) / nbProf(0), "000 000 ms/move") & ", " & Format(nbParties(0), "000 000 games") & ", " & Format(nbProf(0), "00 000 000 moves"))
                Console.WriteLine()
                Console.WriteLine("Averages :")
                Console.WriteLine(Format(totDuree(0) / nbParties(0), "0 000 sec/game") & " (" & Format(totDuree(0) / nbParties(0) / 60, "0 min/game") & ")")
                Console.WriteLine(Format(totCoups / nbParties(0), "000 plies/game") & " (" & Format(nbProf(0) / nbParties(0), "000 played plies/game") & ")")
            End If

        Loop Until lecture.EndOfStream
        lecture.Close()

        Console.Clear()
        Console.Title = My.Computer.Name & " : " & pgn_court & " @ 100%"
        chaine = Format(CInt(tabJoueurs(0)), "000 players")
        If maxLongueur < Len(chaine) Then
            maxLongueur = Len(chaine)
        End If
        Console.WriteLine(chaine & StrDup(maxLongueur - Len(chaine), " ") & " : avg. D" & Format(totProf(0) / nbProf(0), "00") & ", " & Format(1000 * totDuree(0) / nbProf(0), "000 000 ms/move") & ", " & Format(nbParties(0), "000 000 games") & ", " & Format(nbProf(0), "00 000 000 moves"))
        Console.WriteLine()
        chaine = ""
        For i = 1 To CInt(tabJoueurs(0))
            chaine = chaine & tabJoueurs(i) & StrDup(maxLongueur - Len(tabJoueurs(i)), " ") & " : avg. D" & Format(totProf(i) / nbProf(i), "00") & ", " & Format(1000 * totDuree(i) / nbProf(i), "000 000 ms/move") & ", " & Format(nbParties(i), "000 000 games") & ", " & Format(nbProf(i), "00 000 000 moves") & vbCrLf
        Next
        Console.WriteLine(trierChaine(chaine, vbCrLf))
        Console.WriteLine()
        Console.WriteLine("Averages :")
        Console.WriteLine(Format(totDuree(0) / nbParties(0), "0 000 sec/game") & " (" & Format(totDuree(0) / nbParties(0) / 60, "0 min/game") & ")")
        Console.WriteLine(Format(totCoups / nbParties(0), "000 plies/game") & " (" & Format(nbProf(0) / nbParties(0), "000 played plies/game") & ")")
        Console.WriteLine()

        Console.WriteLine("Press Enter to close the window.")
        Console.ReadLine()
    End Sub

    Public Function nomFichier(chemin As String) As String
        Return My.Computer.FileSystem.GetName(chemin)
    End Function

    Public Function trierChaine(serie As String, separateur As String, Optional ordre As Boolean = True) As String
        Dim tabChaine() As String

        tabChaine = Split(serie, separateur)
        If tabChaine(UBound(tabChaine)) = "" Then
            ReDim Preserve tabChaine(UBound(tabChaine) - 1)
        End If

        Array.Sort(tabChaine)
        If Not ordre Then
            Array.Reverse(tabChaine)
        End If

        Return String.Join(separateur, tabChaine)
    End Function

End Module
