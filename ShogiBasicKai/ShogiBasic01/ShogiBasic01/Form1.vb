﻿Imports System.Drawing.Drawing2D
Public Class Form1
    Const DEBUG As Boolean = False
    Const USE_HASH As Boolean = False
    Const KILLER_SORT As Boolean = False
    Const ROBOT_MOVE As Boolean = True
    Const WHITE As Integer = 1
    Const BLACK As Integer = -1
    Const USE_AB As Boolean = True
    Const USE_JYOSEKI As Boolean = False
    Const YOMI_DEPTH As Integer = 3
    Const HAND_RIMIT As Integer = 1
    Const HAND_READ As Boolean = True
    Const NARAZU_READ As Boolean = False
    Const KOMAKIKI_READ As Boolean = True
    Const NIRAMI_READ As Boolean = True
    Const DEBUG_LOG As Boolean = False
    Const RETURN_LOG As Boolean = False
    Const DEBUG_TIME As Boolean = False
    Const ROBO_TEBAN As Integer = WHITE
    Const BLANK As Integer = 255
    Const DUMMY_ID As Integer = 128
    Const BRANCH_WIDTH As Integer = 600
    Const NORMAL_SEARCH = 0
    Const SEARCH_TYPE = NORMAL_SEARCH

    Private CheckBit As Integer = 0
    Class MoveData
        Public komaID As Integer
        Public from As Integer
        Public _to As Integer
        Public hand As Integer = BLANK
        Public classup As Boolean = True
        Public eval As Integer = 0
        Public best_eval As Integer = 0
        Public read_depth As Integer = 0
        Public src_kind As Integer = 0
        Public dst_kind As Integer = 0
        Public teban As Integer = 0
        Public capture As Integer = BLANK
        Public _check As Integer = 0
        Public score As Integer = 0
        Sub New()

        End Sub
        Sub New(ByVal id As Byte,
                ByVal i As Integer,
                ByVal dist As Integer,
                ByVal h As Integer,
                ByVal t As Integer)
            komaID = id
            from = i
            _to = dist
            hand = h
            teban = t
            classup = True
        End Sub

        Sub New(ByVal i As Integer, ByVal j As Integer)
            hand = i
            _to = j
        End Sub

        Public _RetString As String
        Function GetMoveDataString() As String
            _RetString = ""
            _RetString += from.ToString() + ","
            _RetString += _to.ToString() + ","
            _RetString += hand.ToString() + ","
            _RetString += classup.ToString() + ","
            _RetString += eval.ToString() + ","
            _RetString += best_eval.ToString() + ","
            _RetString += read_depth.ToString() + ","
            Return _RetString
        End Function
        Function SetMoveDataFromString(ByVal str As String) As String
            Dim a As Array = str.Split(",")
            If a.Length >= 7 Then
                from = a(0)
                _to = a(1)
                hand = a(2)
                classup = a(3)
            Else
                Return False
            End If
            Return True
        End Function
    End Class
    Dim NOW_TEBAN As Integer = 0
    Dim KikiBlocked As BitBoard
    Dim KomaBlocked(81) As List(Of Integer)
    Dim GenerationFlag As Boolean = False
    Dim Node As List(Of MoveData)
    Dim NodeIdx As Integer
    Dim best As MoveData = New MoveData
    Dim BestScore As Integer = 0
    Dim modosi As Stack(Of MoveData)
    Dim KomaIDNode(40) As List(Of MoveData)
    Dim PosRange(40) As List(Of Integer)
    Dim TegomaRange(40) As List(Of MoveData)
    ''Dim ArrayCount As Integer
    Dim komaname As Array
    Dim board As Array
    Const BB_JOINT As Integer = 63
    Dim POSITION_MAP() As Integer = {8, 7, 6, 5, 4, 3, 2, 1, 0,
                                        17.16, 15, 14, 13, 12, 11, 10, 9,
                                        26, 25, 24, 23, 22, 21, 20, 19, 18,
                                        35, 34, 33, 32, 31, 30, 29, 28, 27,
                                        44, 43, 42, 41, 40, 39, 38, 37, 36,
                                        53, 52, 51, 50, 49, 48, 47, 46, 45,
                                        62, 61, 60.59, 58, 57, 56, 55, 54,
                                        71, 70, 69, 68, 67, 66, 65, 64, 63,
                                        80, 79, 78, 77, 76, 75, 74, 73, 72}
    Class BitBoard
        Public b1 As Int64 = 0
        Public b2 As Int64 = 0
        Public n1 As Int64 = 0
        Public n2 As Int64 = 0
        Private bit As Int64 = 1
        Public Function IsExist(ByVal pos As Int64) As Integer
            Dim b As Int64 = 0
            IsExist = 0
            If 0 <= pos And pos < BB_JOINT Then
                b = b1 And bit << pos
                If b > 0 Then
                    IsExist = 1
                End If
            ElseIf BB_JOINT <= pos And pos <= 80 Then
                b = b2 And bit << (pos - BB_JOINT)
                If b > 0 Then
                    IsExist = 1
                End If
            Else
                IsExist = 0
            End If
        End Function
        Public Function CountBits(ByVal bits As Int64) As Integer
            Dim mask As Int64 = 0
            For i = 0 To BB_JOINT Step 1
                mask = bit << i
                If bits And mask Then
                    Return i
                End If
            Next
            Return -1
        End Function
        Public Function ReverseCount(ByVal bits As Int64) As Integer
            Dim mask As Int64 = 0
            For i = BB_JOINT To 0 Step -1
                mask = bit << i
                If bits And mask Then
                    Return i
                End If
            Next
            Return -1
        End Function
        Public Function GetFirst() As Integer
            n1 = b1
            n2 = b2
            Return GetNext()
        End Function
        Public Function GetNext() As Integer
            Dim ret As Integer = -1
            If 0 <> n1 Then
                ret = CountBits(n1)
                n1 -= bit << ret
            ElseIf 0 <> n2 Then
                ret = CountBits(n2)
                n2 -= bit << ret
                ret += BB_JOINT
            End If
            GetNext = ret
        End Function

        Public Function GetReverse() As Integer
            Dim ret As Integer = -1
            If 0 <> n2 Then
                ret = ReverseCount(n2)
                n2 -= bit << ret
                ret += BB_JOINT
            ElseIf 0 <> n1 Then
                ret = ReverseCount(n1)
                n1 -= bit << ret
            End If
            GetReverse = ret
        End Function
        Public Function GetLast() As Integer
            n1 = b1
            n2 = b2
            Return GetReverse()
        End Function
        Public Function AddBoard(ByVal pos As Integer) As Integer
            If 0 <= pos And pos < BB_JOINT Then
                Dim x As Int64 = bit << pos
                b1 = b1 Or x
            ElseIf BB_JOINT <= pos And pos <= 80 Then
                Dim x As Int64 = bit << (pos - BB_JOINT)
                b2 = b2 Or x
            Else
                AddBoard = -1
                Return AddBoard
            End If
            Return 0
        End Function
        Public Function RemoveBoard(ByVal pos As Integer) As Integer
            If 0 <= pos And pos < BB_JOINT Then
                Dim x As Int64 = bit << pos
                x = Not x
                b1 = b1 And x
            ElseIf BB_JOINT <= pos And pos <= 80 Then
                Dim x As Int64 = bit << (pos - BB_JOINT)
                x = Not x
                b2 = b2 And x
            Else
                RemoveBoard = -1
                Return RemoveBoard
            End If
            Return 0
        End Function
    End Class
    Const W_OR_B As Integer = 2
    Const KOMA_KIND As Integer = 28
    Const KOMA_POS As Integer = 81
    Class KiKiBoard
        Private bit As Int64 = 1
        Private Hu(W_OR_B, KOMA_POS) As BitBoard
        Private Kei(W_OR_B, KOMA_POS) As BitBoard
        Private Gin(W_OR_B, KOMA_POS) As BitBoard
        Private Kin(W_OR_B, KOMA_POS) As BitBoard
        Private Ou(W_OR_B, KOMA_POS) As BitBoard
        Private Kyo(W_OR_B, KOMA_POS) As BitBoard
        Sub New()
            For i = 0 To W_OR_B - 1 Step 1
                For j = 0 To KOMA_POS - 1 Step 1
                    Hu(i, j) = New BitBoard
                    Kei(i, j) = New BitBoard
                    Gin(i, j) = New BitBoard
                    Kin(i, j) = New BitBoard
                    Ou(i, j) = New BitBoard
                    Kyo(i, j) = New BitBoard
                Next
            Next
        End Sub
        Public Function AndBB(ByVal bb1 As BitBoard,
                        ByVal bb2 As BitBoard) As BitBoard
            AndBB = New BitBoard
            AndBB.b1 = bb1.b1 And bb2.b1
            AndBB.b2 = bb1.b2 And bb2.b2
            Return AndBB
        End Function
        Public Function XorBB(ByVal bb1 As BitBoard,
                        ByVal bb2 As BitBoard) As BitBoard
            XorBB = New BitBoard
            XorBB.b1 = bb1.b1 Xor bb2.b1
            XorBB.b2 = bb1.b2 Xor bb2.b2
            Return XorBB
        End Function
        Private Function GetWB(ByVal wb As Integer) As Integer
            If wb = WHITE Then
                Return 0
            End If
            If wb = BLACK Then
                Return 1
            End If
            Return 2 'Error
        End Function
        Private Function CheckBoardRange(ByVal x As Integer, ByVal y As Integer) As Boolean
            CheckBoardRange = True
            If x < 0 Or 8 < x Then
                CheckBoardRange = False
            End If
            If y < 0 Or 8 < y Then
                CheckBoardRange = False
            End If
        End Function
        Public Sub AddHuRange(ByVal locate As Integer,
                                    ByVal wb As Integer,
                                    ByVal dx As Integer,
                                    ByVal dy As Integer)
            Dim dist As Integer
            If CheckBoardRange(dx, dy) = True Then
                dist = dx + dy * 9
                Hu(GetWB(wb), locate).AddBoard(dist)
            End If
        End Sub
        Public Sub AddKinRange(ByVal locate As Integer,
                                    ByVal wb As Integer,
                                    ByVal dx As Integer,
                                    ByVal dy As Integer)
            Dim dist As Integer
            If CheckBoardRange(dx, dy) = True Then
                dist = dx + dy * 9
                Kin(GetWB(wb), locate).AddBoard(dist)
            End If
        End Sub
        Public Sub AddGinRange(ByVal locate As Integer,
                                    ByVal wb As Integer,
                                    ByVal dx As Integer,
                                    ByVal dy As Integer)
            Dim dist As Integer
            If CheckBoardRange(dx, dy) = True Then
                dist = dx + dy * 9
                Gin(GetWB(wb), locate).AddBoard(dist)
            End If
        End Sub
        Public Sub AddKeiRange(ByVal locate As Integer,
                                    ByVal wb As Integer,
                                    ByVal dx As Integer,
                                    ByVal dy As Integer)
            Dim dist As Integer
            If CheckBoardRange(dx, dy) = True Then
                dist = dx + dy * 9
                Kei(GetWB(wb), locate).AddBoard(dist)
            End If
        End Sub
        Public Sub AddOuRange(ByVal locate As Integer,
                                    ByVal wb As Integer,
                                    ByVal dx As Integer,
                                    ByVal dy As Integer)
            Dim dist As Integer
            If CheckBoardRange(dx, dy) = True Then
                dist = dx + dy * 9
                Ou(GetWB(wb), locate).AddBoard(dist)
            End If
        End Sub
        Public Sub AddKyoRange(ByVal locate As Integer,
                                    ByVal wb As Integer,
                                    ByVal dx As Integer,
                                    ByVal dy As Integer)
            Dim dist As Integer
            If CheckBoardRange(dx, dy) = True Then
                dist = dx + dy * 9
                Kyo(GetWB(wb), locate).AddBoard(dist)
            End If
        End Sub
        Public Function GetHuRange(ByVal locate As Integer,
                                    ByVal wb As Integer,
                                    ByVal bb As BitBoard,
                                    ByRef block As BitBoard)
            block = AndBB(Hu(GetWB(wb), locate), bb)
            Dim BufBB = XorBB(Hu(GetWB(wb), locate), bb)
            Dim RetBB = AndBB(Hu(GetWB(wb), locate), BufBB)
            Dim RetValue As Integer = RetBB.GetFirst()
            If RetValue = -1 Then
                Return BLANK
            End If
            Return RetValue
        End Function
        Public Function GetKinRange(ByVal locate As Integer,
                                    ByVal wb As Integer,
                                    ByVal bb As BitBoard,
                                    ByRef block As BitBoard) As BitBoard
            block = AndBB(Kin(GetWB(wb), locate), bb)
            Dim BufBB = XorBB(Kin(GetWB(wb), locate), bb)
            Dim RetBB = AndBB(Kin(GetWB(wb), locate), BufBB)
            Return RetBB
        End Function
        Public Function GetGinRange(ByVal locate As Integer,
                                    ByVal wb As Integer,
                                    ByVal bb As BitBoard,
                                    ByRef block As BitBoard) As BitBoard
            block = AndBB(Gin(GetWB(wb), locate), bb)
            Dim BufBB = XorBB(Gin(GetWB(wb), locate), bb)
            Dim RetBB = AndBB(Gin(GetWB(wb), locate), BufBB)
            Return RetBB
        End Function
        Public Function GetKeiRange(ByVal locate As Integer,
                                    ByVal wb As Integer,
                                    ByVal bb As BitBoard,
                                    ByRef block As BitBoard) As BitBoard
            block = AndBB(Kei(GetWB(wb), locate), bb)
            Dim BufBB = XorBB(Kei(GetWB(wb), locate), bb)
            Dim RetBB = AndBB(Kei(GetWB(wb), locate), BufBB)
            Return RetBB
        End Function
        Public Function GetOuRange(ByVal locate As Integer,
                                    ByVal wb As Integer,
                                    ByVal bb As BitBoard,
                                    ByRef block As BitBoard) As BitBoard
            block = AndBB(Ou(GetWB(wb), locate), bb)
            Dim BufBB = XorBB(Ou(GetWB(wb), locate), bb)
            Dim RetBB = AndBB(Ou(GetWB(wb), locate), BufBB)
            Return RetBB
        End Function
        Public Function GetKyoRange(ByVal locate As Integer,
                                    ByVal wb As Integer,
                                    ByVal bb_w As BitBoard,
                                    ByVal bb_b As BitBoard) As BitBoard
            Dim BufBB_W = AndBB(Kyo(GetWB(wb), locate), bb_w)
            Dim BufBB_B = AndBB(Kyo(GetWB(wb), locate), bb_b)
            Dim RetBB = CutRange(locate, wb, Kyo(GetWB(wb), locate), BufBB_W, BufBB_B)
            Return RetBB
        End Function
        Public Function CutRange(ByVal locate As Integer,
                                    ByVal wb As Integer,
                                    ByVal bb_k As BitBoard,
                                    ByVal bb_w As BitBoard,
                                    ByVal bb_b As BitBoard) As BitBoard
            Dim All_F As Int64 = -1
            Dim RetBB = bb_k
            If wb = BLACK Then
                Dim bb_c As BitBoard = AndBB(bb_k, bb_b)
                Dim pos = bb_b.GetFirst()
                If pos <> -1 Then
                    If 0 <= pos And pos < BB_JOINT Then
                        Dim x As Int64 = bit << pos
                        bb_c.b1 = x - 1
                        bb_c.b2 = 0
                    ElseIf BB_JOINT <= pos And pos <= 80 Then
                        Dim x As Int64 = bit << (pos - BB_JOINT)
                        bb_c.b1 = All_F
                        bb_c.b2 = x - 1
                    End If
                    RetBB = AndBB(bb_k, bb_c)
                End If
                bb_c = AndBB(RetBB, bb_w)
                pos = bb_c.GetFirst()
                If pos <> -1 Then
                    If 0 <= pos And pos < BB_JOINT Then
                        Dim x As Int64 = bit << pos
                        bb_c.b1 = (x - 1) + x
                        bb_c.b2 = 0
                    ElseIf BB_JOINT <= pos And pos <= 80 Then
                        Dim x As Int64 = bit << (pos - BB_JOINT)
                        bb_c.b1 = All_F
                        bb_c.b2 = (x - 1) + x
                    End If
                    RetBB = AndBB(RetBB, bb_c)
                End If
            Else
                Dim bb_c As BitBoard = AndBB(bb_k, bb_w)
                Dim pos = bb_c.GetLast()
                If pos <> -1 Then
                    If 0 <= pos And pos < BB_JOINT Then
                        Dim x As Int64 = bit << pos
                        bb_c.b1 = (x - 1) + x
                    ElseIf BB_JOINT <= pos And pos <= 80 Then
                        Dim x As Int64 = bit << (pos - BB_JOINT)
                        bb_c.b1 = All_F
                        bb_c.b2 = (x - 1) + x
                    End If
                    bb_c.b1 = Not bb_c.b1
                    bb_c.b2 = Not bb_c.b2
                    RetBB = AndBB(bb_k, bb_c)
                End If
                bb_c = AndBB(RetBB, bb_b)
                pos = bb_c.GetLast()
                If pos <> -1 Then
                    If 0 <= pos And pos < BB_JOINT Then
                        Dim x As Int64 = bit << pos
                        bb_c.b1 = x - 1
                    ElseIf BB_JOINT <= pos And pos <= 80 Then
                        Dim x As Int64 = bit << (pos - BB_JOINT)
                        bb_c.b1 = All_F
                        bb_c.b2 = x - 1
                    End If
                    bb_c.b1 = Not bb_c.b1
                    bb_c.b2 = Not bb_c.b2
                    RetBB = AndBB(RetBB, bb_c)
                End If
            End If
            Return RetBB
        End Function
        Public Sub GetHuKiki(ByVal locate As Integer,
                                    ByVal wb As Integer,
                                    ByVal bb_w As BitBoard,
                                    ByVal bb_b As BitBoard,
                                    ByRef kk_w As Array,
                                    ByRef kk_b As Array)
            Dim BufBB = Hu(GetWB(wb), locate)
            GetKomaKiki(BufBB, locate, wb, bb_w, bb_b, kk_w, kk_b)
        End Sub
        Public Sub GetKinKiki(ByVal locate As Integer,
                                    ByVal wb As Integer,
                                    ByVal bb_w As BitBoard,
                                    ByVal bb_b As BitBoard,
                                    ByRef kk_w As Array,
                                    ByRef kk_b As Array)
            Dim BufBB = Kin(GetWB(wb), locate)
            GetKomaKiki(BufBB, locate, wb, bb_w, bb_b, kk_w, kk_b)
        End Sub
        Public Sub GetGinKiki(ByVal locate As Integer,
                                    ByVal wb As Integer,
                                    ByVal bb_w As BitBoard,
                                    ByVal bb_b As BitBoard,
                                    ByRef kk_w As Array,
                                    ByRef kk_b As Array)
            Dim BufBB = Gin(GetWB(wb), locate)
            GetKomaKiki(BufBB, locate, wb, bb_w, bb_b, kk_w, kk_b)
        End Sub
        Public Sub GetKeiKiki(ByVal locate As Integer,
                                    ByVal wb As Integer,
                                    ByVal bb_w As BitBoard,
                                    ByVal bb_b As BitBoard,
                                    ByRef kk_w As Array,
                                    ByRef kk_b As Array)
            Dim BufBB = Kei(GetWB(wb), locate)
            GetKomaKiki(BufBB, locate, wb, bb_w, bb_b, kk_w, kk_b)
        End Sub
        Public Sub GetOuKiki(ByVal locate As Integer,
                                    ByVal wb As Integer,
                                    ByVal bb_w As BitBoard,
                                    ByVal bb_b As BitBoard,
                                    ByRef kk_w As Array,
                                    ByRef kk_b As Array)
            Dim BufBB = Ou(GetWB(wb), locate)
            GetKomaKiki(BufBB, locate, wb, bb_w, bb_b, kk_w, kk_b)
        End Sub
        Public Sub GetKyoKiki(ByVal locate As Integer,
                                    ByVal wb As Integer,
                                    ByVal bb_w As BitBoard,
                                    ByVal bb_b As BitBoard,
                                    ByRef kk_w As Array,
                                    ByRef kk_b As Array)
            Dim BufBB = Kyo(GetWB(wb), locate)
            Dim RetBB As BitBoard
            Dim RetValue As Integer
            If wb = WHITE Then
                RetBB = AndBB(BufBB, bb_w)
                RetValue = RetBB.GetLast()
                If RetValue <> -1 Then
                    kk_w(RetValue) += 1
                End If
                RetBB = AndBB(BufBB, bb_b)
                RetValue = RetBB.GetLast()
                If RetValue <> -1 Then
                    kk_b(RetValue) += 1
                End If
            ElseIf wb = BLACK Then
                RetBB = AndBB(BufBB, bb_b)
                RetValue = RetBB.GetFirst()
                If RetValue <> -1 Then
                    kk_b(RetValue) += 1
                End If
                RetBB = AndBB(BufBB, bb_w)
                RetValue = RetBB.GetFirst()
                If RetValue <> -1 Then
                    kk_w(RetValue) += 1
                End If
            End If
        End Sub
        Public Sub GetKomaKiki(ByVal BufBB As BitBoard,
                                ByVal locate As Integer,
                                    ByVal wb As Integer,
                                    ByVal bb_w As BitBoard,
                                    ByVal bb_b As BitBoard,
                                    ByRef kk_w As Array,
                                    ByRef kk_b As Array)
            Dim RetBB As BitBoard
            Dim RetValue As Integer
            If wb = WHITE Then
                RetBB = AndBB(BufBB, bb_w)
                RetValue = RetBB.GetFirst()
                While RetValue <> -1
                    kk_w(RetValue) += 1
                    RetValue = RetBB.GetNext()
                End While
            ElseIf wb = BLACK Then
                RetBB = AndBB(BufBB, bb_b)
                RetValue = RetBB.GetFirst()
                While RetValue <> -1
                    kk_b(RetValue) += 1
                    RetValue = RetBB.GetNext()
                End While
            End If
        End Sub
    End Class
    Dim bb_white As BitBoard = New BitBoard
    Dim bb_black As BitBoard = New BitBoard
    Dim bb_kiki As KiKiBoard = New KiKiBoard
    Dim state As Integer
    Const ST_FREE As Integer = 0
    Const ST_WHITE_CHOOSE As Integer = 1
    Const ST_WHITE_MOVE As Integer = 3
    Const ST_BLACK_CHOOSE As Integer = 2
    Const ST_BLACK_MOVE As Integer = 4
    Dim undo As Integer
    Dim range(81) As Integer
    Dim tegomaw As List(Of Integer)
    Dim tegomab As List(Of Integer)
    Private Function GetTegoma(ByVal it As Integer, ByVal wb As Integer) As Integer
        If wb = WHITE And DUMMY_ID <> tegomaw(it) Then
            Dim p As PieceID = Piece(tegomaw(it))
            If p.id <> tegomaw(it) Then
                Return DUMMY_ID
            End If
            Dim ret = p.omote
            Return ret
        ElseIf wb = BLACK And DUMMY_ID <> tegomab(it) Then
            Dim p As PieceID = Piece(tegomab(it))
            If p.id <> tegomab(it) Then
                Return DUMMY_ID
            End If
            Dim ret = p.omote
            Return ret
        End If
        Return DUMMY_ID
    End Function
    Private Function GetTegomaIDFromKind(ByVal k As Integer, ByVal wb As Integer) As Integer
        If wb = WHITE Then
            For i = 0 To tegomaw.Count - 1 Step 1
                Dim koma = Piece(tegomaw(i)).omote
                If koma = k Then
                    Dim p As PieceID = Piece(tegomaw(i))
                    Return p.id
                End If
            Next
        Else
            For i = 0 To tegomab.Count - 1 Step 1
                Dim koma = Piece(tegomab(i)).omote + 14
                If koma = k Then
                    Dim p As PieceID = Piece(tegomab(i))
                    Return p.id
                End If
            Next
        End If
        Return DUMMY_ID
    End Function
    Private Function KomaIdx(ByVal i As Integer, ByVal wb As Integer) As Integer
        Static ARW As Array = {1, 2, 3, 4, 5, 6, 7, 8}
        Static ARB As Array = {15, 16, 17, 18, 19, 20, 21, 22}
        If wb = WHITE Then
            Return ARW(i)
        Else
            Return ARB(i)
        End If
    End Function
    Dim pop As Integer
    Dim pop_id As Integer
    Dim all As Array
    Dim komaundo As Integer
    Dim kihumem As Integer
    Dim narimem As Integer
    Dim robomode As Boolean
    Dim komakiki_w As Array
    Dim komakiki_b As Array
    Dim nirami_w As Integer
    Dim nirami_b As Integer
    Const KOMAKIKI_SUM As Integer = 11
    Const WH_OR_BL As Integer = 2 '空きマスを考慮する場合は"3"
    Const KING_POS_MAX As Integer = 81
    Const EFFECT_POS As Integer = 81
    Dim FINISH_SCORE = 9000
    Dim koma_position_score(KOMA_KIND, KOMA_POS) As Integer
    Const POSITION_BIAS As Integer = 150
    Const HIGH_POSITION_SCORE As Integer = 1
    Dim our_effect_value(9) As Integer
    Dim their_effect_value(9) As Integer
    Dim blank_effect_value(9) As Integer
    Dim score_table(KOMAKIKI_SUM, WH_OR_BL, KING_POS_MAX, EFFECT_POS) As Integer
    Class EvalBuff
        Public komatoku As Integer = 0
        Public komakiki As Integer = 0
        Public komaichi As Integer = 0
        Public Sub Init()
            komatoku = 0
            komakiki = 0
            komaichi = 0
        End Sub
    End Class
    Private currentEval As Integer ' 現在の評価値
    Private WBufGlobal As EvalBuff = New EvalBuff() ' 先手の評価（komatoku, komaichi, komakiki）
    Private BBufGlobal As EvalBuff = New EvalBuff() ' 後手の評価
    Sub InitializeEval()
        currentEval = Hyouka()
        WBufGlobal = WBuf
        BBufGlobal = BBuf
    End Sub
    Sub UpdateKingPos(ByVal move As MoveData)
        If board(move.from) = 8 Then king_pos = move._to
        If board(move.from) = 22 Then enem_pos = move._to
    End Sub
    Public Structure Buffer
        Public komatoku As Integer ' 駒の価値の合計
        Public komaichi As Integer ' 駒の位置評価の合計
        Public komakiki As Integer ' 駒の利きの評価の合計

        ' 初期化メソッド
        Public Sub Init()
            komatoku = 0
            komaichi = 0
            komakiki = 0
        End Sub
    End Structure
    Dim king_pos As Integer
    Dim enem_pos As Integer

    Private Sub UpdateEval(ByVal move As MoveData, ByRef deltaW As Buffer, ByRef deltaB As Buffer)
        deltaW.Init()
        deltaB.Init()
        If DEBUG Then
            ' インデックスチェック
            If move.from < 0 Or move.from > 80 Then
                Throw New ArgumentException("move.fromが範囲外: " & move.from)
            End If
            If move._to < 0 Or move._to > 80 Then
                Throw New ArgumentException("move.toが範囲外: " & move._to)
            End If
            If king_pos < 0 Or king_pos > 80 Then
                Throw New ArgumentException("king_posが範囲外: " & king_pos)
            End If
            If enem_pos < 0 Or enem_pos > 80 Then
                Throw New ArgumentException("enem_posが範囲外: " & enem_pos)
            End If
        End If
        If king_pos < 0 Or king_pos > 80 Then
            currentEval = -(FINISH_SCORE * 200)
            Exit Sub
        End If
        If enem_pos < 0 Or enem_pos > 80 Then
            currentEval = FINISH_SCORE * 200
            Exit Sub
        End If

        ' 移動する駒（移動先の駒は成り後の可能性あり）
        Dim originalPiece As Integer = move.src_kind ' MoveDataに移動前の駒を仮定
        Dim movingPiece As Integer = board(move._to)  ' 移動後の駒（成り済み）

        ' 移動元の評価を引く
        If IsWB(WHITE, move._to) Then
            deltaW.komatoku -= KomaScore(originalPiece) * 100
            deltaW.komaichi -= koma_position_score(originalPiece, move.from) * 100
            deltaW.komakiki -= score_table(komakiki_w(move.from), 1, king_pos, move.from) * 100
            deltaB.komakiki -= score_table(komakiki_b(move.from), 1, enem_pos, move.from) * 100
        Else
            deltaB.komatoku -= KomaScore(originalPiece) * 100
            deltaB.komaichi -= koma_position_score(originalPiece, move.from) * 100
            deltaW.komakiki -= score_table(komakiki_w(move.from), 1, king_pos, move.from) * 100
            deltaB.komakiki -= score_table(komakiki_b(move.from), 0, king_pos, move.from) * 100
        End If

        ' 移動先の評価を足す（成り考慮）
        If IsWB(WHITE, move._to) Then
            deltaW.komatoku += KomaScore(movingPiece) * 100 ' 成駒のスコア
            deltaW.komaichi += koma_position_score(movingPiece, move._to) * 100
            deltaW.komakiki += score_table(komakiki_w(move._to), 1, king_pos, move._to) * 100
            deltaB.komakiki += score_table(komakiki_b(move._to), 1, enem_pos, move._to) * 100
        Else
            deltaB.komatoku += KomaScore(movingPiece) * 100
            deltaB.komaichi += koma_position_score(movingPiece, move._to) * 100
            deltaW.komakiki += score_table(komakiki_w(move._to), 1, king_pos, move._to) * 100
            deltaB.komakiki += score_table(komakiki_b(move._to), 0, king_pos, move._to) * 100
        End If

        ' 捕獲があれば持ち駒として加算
        If move.capture <> DUMMY_ID And 0 < move.capture And move.capture <= Piece.Count Then
            If move.teban = WHITE Then
                deltaW.komatoku += KomaScore(Piece(move.capture).omote) * 105
                deltaB.komatoku -= KomaScore(Piece(move.capture).omote) * 105
            Else
                deltaW.komatoku -= KomaScore(Piece(move.capture).omote) * 105
                deltaB.komatoku += KomaScore(Piece(move.capture).omote) * 105
            End If
        End If

        ' グローバル評価値を更新
        currentEval += (deltaW.komatoku + deltaW.komaichi + deltaW.komakiki)
        currentEval -= (deltaB.komatoku + deltaB.komaichi + deltaB.komakiki)

        ' グローバルバッファを更新
        'WBufGlobal.komatoku += deltaW.komatoku
        'WBufGlobal.komaichi += deltaW.komaichi
        'WBufGlobal.komakiki += deltaW.komakiki
        'BBufGlobal.komatoku += deltaB.komatoku
        'BBufGlobal.komaichi += deltaB.komaichi
        'BBufGlobal.komakiki += deltaB.komakiki
    End Sub

    Private Sub UnmakeEval(ByVal deltaW As Buffer, ByVal deltaB As Buffer)
        currentEval -= (deltaW.komatoku + deltaW.komaichi + deltaW.komakiki)
        currentEval += (deltaB.komatoku + deltaB.komaichi + deltaB.komakiki)

        'WBufGlobal.komatoku -= deltaW.komatoku
        'WBufGlobal.komaichi -= deltaW.komaichi
        'WBufGlobal.komakiki -= deltaW.komakiki
        'BBufGlobal.komatoku -= deltaB.komatoku
        'BBufGlobal.komaichi -= deltaB.komaichi
        'BBufGlobal.komakiki -= deltaB.komakiki
    End Sub

    Dim WBuf As EvalBuff = New EvalBuff() 'WhiteBuff
    Dim BBuf As EvalBuff = New EvalBuff() 'BlackBuff
    Dim WTop As EvalBuff = New EvalBuff()
    Dim BTop As EvalBuff = New EvalBuff()

    Dim allPieces As New List(Of Integer) From {
        1, 1, 1, 1, 1, 1, 1, 1, 1, ' 先手の歩9枚
        2, 3, 4, 5, 8, 5, 4, 3, 2, ' 先手の香桂銀金王金銀桂香
        15, 15, 15, 15, 15, 15, 15, 15, 15, ' 後手の歩9枚
        16, 17, 18, 19, 22, 19, 18, 17, 16 ' 後手の香桂銀金王金銀桂香
    }

    Private Function GetLostID(ByVal take As Integer, ByVal pos As Integer) As Integer
        For i = 0 To Piece.Count - 1
            Dim p As PieceID = Piece(i)
            If p.kind = take And p.place = pos Then
                Return Piece(i).id
            End If
        Next
        For n = 0 To 80
            For i = 0 To Piece.Count - 1
                Dim p As PieceID = Piece(i)
                If 0 <= p.place And p.place <= 80 Then
                    If p.kind <> board(p.place) Then

                    End If
                End If
            Next
        Next

        Return DUMMY_ID
    End Function
    Function GetCurrentPieces() As List(Of Integer)
        Dim current As New List(Of Integer)
        ' 盤面の駒
        For i = 0 To 80
            If board(i) <> 0 Then current.Add(board(i))
        Next
        ' 先手の手駒
        current.AddRange(tegomaw)

        ' 後手の手駒
        current.AddRange(tegomab)

        Return current
    End Function
    Sub IdentifyMissingOrExtra()
        If DEBUG = False Then
            Exit Sub
        End If
        Dim total = CalculateSum()
        If total <> 40 Then
            Dim currentPieces = GetCurrentPieces()
            Console.WriteLine("異常: SUM = " & total)

            If total < 40 Then
                ' 失くした駒を特定
                For Each id In allPieces
                    If Not currentPieces.Contains(id) Then
                        Console.WriteLine("失くした駒ID: " & id)
                        Exit For ' 1つ特定したら終了（仮定）
                    End If
                Next
            ElseIf total > 40 Then
                ' 増えた駒を特定
                Dim tempList As New List(Of Integer)(allPieces)
                For Each id In currentPieces
                    If tempList.Contains(id) Then
                        tempList.Remove(id) ' 一致したら削除
                    Else
                        Console.WriteLine("増えた駒ID: " & id)
                        ' 重複チェック用にリストに残す
                    End If
                Next
            End If
        End If
    End Sub

    Private Function SetBoard(ByVal from As Integer, ByVal dist As Integer, ByVal id As Integer) As Integer
        Dim koma = BLANK
        If from = BLANK Then
            Console.WriteLine("SetBoard: From Hand" & ":from:" & from.ToString & ",dist:" & dist.ToString & ",id:" & id.ToString)
            koma = Piece(id).kind
        Else
            koma = board(from)
        End If
        If id <> DUMMY_ID Then
            Dim c_up = Piece(id).kind
            If 1 <= koma And koma <= 14 And 1 <= c_up And c_up <= 14 Then
                koma = c_up
            ElseIf 15 <= koma And koma <= 28 And 15 <= c_up And c_up <= 28 Then
                koma = c_up
            Else
                Console.WriteLine("Cross Koma Move at SetBoard !")
            End If
        Else
            Console.WriteLine("DummyID at SetBoard !")
        End If
        Dim take = board(dist)
        Dim dst_id = DUMMY_ID
        If 0 < take Then
            dst_id = FindID(dist)
            If id = DUMMY_ID Then
                id = FromKind(take, dist)
            End If
        End If
        If 1 <= koma And koma <= 14 Then
            If take > 0 And dst_id <> DUMMY_ID Then
                AddTegomaW(dst_id)
                Piece(dst_id).place = BLANK
                Piece(dst_id).captured = WHITE
            ElseIf take > 0 Then
                'AddTegomaW()
            End If
            board(dist) = koma
            bb_white.AddBoard(dist)
            bb_black.RemoveBoard(dist)
        End If
        If 15 <= koma Then
            If take > 0 And dst_id <> DUMMY_ID Then
                AddTegomaB(dst_id)
                Piece(dst_id).place = BLANK
                Piece(dst_id).captured = BLACK
            ElseIf take > 0 Then
                Console.WriteLine("Failed Take at SetBoard !")
                'AddTegomaW()
            End If
            board(dist) = koma
            bb_black.AddBoard(dist)
            bb_white.RemoveBoard(dist)
        End If
        Piece(id).place = dist
        If from <> BLANK Then
            board(from) = 0
            bb_white.RemoveBoard(from)
            bb_black.RemoveBoard(from)
        Else
            Console.WriteLine("SetBoard: from BLANK")
            board(dist) = 0
            bb_white.RemoveBoard(dist)
            bb_black.RemoveBoard(dist)
        End If
        Return dst_id
    End Function
    Private Function SetBoardKind(ByVal from As Integer, ByVal dist As Integer, ByVal id As Integer, ByVal kind As Integer) As Integer
        Dim koma = kind
        Dim back = -1
        If from <> 255 Then
            back = board(from)
        End If
        If back <> -1 And back <> koma Then
            Console.WriteLine("SetBoardKind Diff from koma")
        End If
        If 1 <= koma And koma <= 14 Then
            board(dist) = koma
            bb_white.AddBoard(dist)
            bb_black.RemoveBoard(dist)
        End If
        If 15 <= koma Then
            board(dist) = koma
            bb_black.AddBoard(dist)
            bb_white.RemoveBoard(dist)
        End If
        If id <> DUMMY_ID Then
            Piece(id).place = dist
        End If
        If from <> BLANK Then
            board(from) = 0
            bb_white.RemoveBoard(from)
            bb_black.RemoveBoard(from)
        End If
        Return 0
    End Function
    Class PieceID
        Public id As Integer = -1
        Public kind As Integer = 0
        Public omote As Integer = 0
        Public place As Integer = BLANK
        Public owner As Integer = 0
        Public captured As Integer = 0
    End Class
    Dim Piece As List(Of PieceID)
    Private Function FromKind(ByVal k As Integer, ByVal pos As Integer)
        Dim ret = DUMMY_ID
        Dim kind = k
        If kind >= 15 Then
            kind -= 14
        End If
        kind = ClassDown(kind)
        Dim max = 80
        For i = 0 To Piece.Count - 1
            If Piece(i).omote = kind Then
                Dim dist = Math.Abs(Piece(i).place - pos)
                If (dist < max) Then
                    max = dist
                    ret = Piece(i).id
                End If
            End If
        Next
        Return ret
    End Function

    Private Function FindID(ByVal pos As Integer)
        For i = 0 To Piece.Count - 1
            If Piece(i).place = pos Then
                Return Piece(i).id
            End If
        Next
        Console.WriteLine("FindID Missing position = " & pos.ToString)
        Return DUMMY_ID
    End Function
    Private Function FindIDPop(ByVal pop As Integer, ByVal wb As Integer)
        For i = 0 To Piece.Count - 1
            If Piece(i).kind = pop Then
                Return Piece(i).id
            End If
        Next
        Return DUMMY_ID
    End Function
    Class ShogiBoard
        Public _all = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80}
        Public _old_board = {16, 17, 18, 19, 22, 19, 18, 17, 16,
                    0, 21, 0, 0, 0, 0, 0, 20, 0,
                    15, 15, 15, 15, 15, 15, 15, 15, 15,
                    0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0,
                    1, 1, 1, 1, 1, 1, 1, 1, 1,
                    0, 6, 0, 0, 0, 0, 0, 7, 0,
                    2, 3, 4, 5, 8, 5, 4, 3, 2}
        Public _new_board = {1, 2, 3, 4, 5, 6, 7, 8, 9,
                    0, 10, 0, 0, 0, 0, 0, 11, 0,
                    12, 13, 14, 15, 16, 17, 18, 19, 20,
                    0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0,
                    21, 22, 23, 24, 25, 26, 27, 28, 29,
                    0, 30, 0, 0, 0, 0, 0, 31, 0,
                    32, 33, 34, 35, 36, 37, 38, 39, 40}
        Public _board(9, 9) As Integer
        Public _retstring As String
        Public _id = {0, 1, 2, 3, 4, 5, 6, 7, 8,
            128, 9, 128, 128, 128, 128, 128, 10, 128,
            11, 12, 13, 14, 15, 16, 17, 18, 19,
            128, 128, 128, 128, 128, 128, 128, 128, 128,
            128, 128, 128, 128, 128, 128, 128, 128, 128,
            128, 128, 128, 128, 128, 128, 128, 128, 128,
            20, 21, 22, 23, 24, 25, 26, 27, 28,
            128, 29, 128, 128, 128, 128, 128, 30, 128,
            31, 32, 33, 34, 35, 36, 37, 38, 39}

        Public Function GetBoardString(ByVal bd As Array) As String
            _old_board = bd
            _retstring = ""
            For i = 0 To 80
                _retstring += bd(i).ToString() + ","
            Next
            'For i = 0 To 7
            '_retstring += _tegomaw(i).ToString() + ","
            'Next
            'For i = 0 To 7
            '_retstring += _tegomab(i).ToString() + ","
            'Next
            Return _retstring
        End Function
    End Class
    Public _b As ShogiBoard = New ShogiBoard()
    Public _JyosekiDictionary As Dictionary(Of String, String) = New Dictionary(Of String, String)
    Private Sub Init() Handles Me.HandleCreated
        Piece = New List(Of PieceID)
        Node = New List(Of MoveData)
        For n = 0 To (BRANCH_WIDTH * (YOMI_DEPTH + 1)) Step 1
            Node.Add(New MoveData())
        Next
        For n = 0 To 40 - 1 Step 1
            KomaIDNode(n) = New List(Of MoveData)
            PosRange(n) = New List(Of Integer)
            TegomaRange(n) = New List(Of MoveData)
        Next
        For n = 0 To 81 - 1 Step 1
            KomaBlocked(n) = New List(Of Integer)
        Next
        For n = 0 To 40 - 1
            For m = 0 To 81 - 1
                TegomaRange(n).Add(New MoveData(n, m))
            Next
        Next
        modosi = New Stack(Of MoveData)

        komaname = {"", "歩", "香", "桂", "銀", "金", "飛", "角", "王", "と", "杏", "圭", "全", "龍", "馬"}
        all = _b._all
        board = _b._old_board
        tegomaw = New List(Of Integer)
        tegomab = New List(Of Integer)
        Dim id = 0
        For i = 0 To 80 Step 1
            Dim k = board(i)
            If k <> 0 Then
                Dim pi As PieceID = New PieceID
                pi.id = id
                pi.kind = k
                Dim omt = k
                If omt >= 15 Then
                    omt -= 14
                End If
                pi.omote = omt
                pi.place = i
                pi.captured = 0
                If k <= 14 Then
                    pi.owner = WHITE
                Else
                    pi.owner = BLACK
                End If
                Piece.Add(pi)
                id += 1
            End If
        Next

        'Dim s As String = _b.GetBoardString(board)
        For i = 0 To 80 Step 1
            Dim k = board(i)
            If k <> 0 Then
                If k <= 14 Then
                    bb_white.AddBoard(i)
                Else
                    bb_black.AddBoard(i)
                End If
            End If
        Next
        state = ST_FREE
        undo = BLANK
        komaundo = BLANK
        kihumem = 0
        narimem = BLANK
        robomode = False
        KomaKikiInit()
        For d = 0 To 8 Step 1
            our_effect_value(d) = 1 * 1024 / (d + 1)
            their_effect_value(d) = 24 * 1024 / (d + 1)
            blank_effect_value(d) = 36 * 1024 / (d + 1)
        Next
        Dim k_sum_val(KOMAKIKI_SUM) As Integer
        For m = 0 To KOMAKIKI_SUM - 1 Step 1
            If m = 0 Then
                k_sum_val(m) = 0
            Else
                k_sum_val(m) = (1024 * Math.Pow(1.2, (m - 1))) / 1024
            End If
        Next
        For m = 0 To KOMAKIKI_SUM - 1 Step 1
            For kp = 0 To 80 Step 1
                For i = 0 To 80 Step 1
                    score_table(m, 0, kp, i) = k_sum_val(m) * our_effect_value(KomaDist(kp, i)) / 1024
                    score_table(m, 1, kp, i) = k_sum_val(m) * their_effect_value(KomaDist(kp, i)) / 1024
                    score_table(m, 2, kp, i) = k_sum_val(m) * blank_effect_value(KomaDist(kp, i)) / 1024
                Next
            Next
        Next
        For m = 0 To KOMA_KIND - 1 Step 1
            For n = 0 To KOMA_POS - 1 Step 1
                koma_position_score(m, n) = 0 '(n Mod 9) * HIGH_POSITION_SCORE
            Next
        Next
        '穴熊定跡
        koma_position_score(22, 0) = 100
        koma_position_score(22, 1) = 40
        koma_position_score(22, 2) = 20
        koma_position_score(22, 3) = 20
        koma_position_score(22, 10) = 60
        koma_position_score(22, 11) = 40
        koma_position_score(22, 12) = 20
        koma_position_score(22, 13) = -5
        '金銀は矢倉を目指す
        koma_position_score(19, 10) = 65
        koma_position_score(19, 11) = 60
        koma_position_score(19, 12) = 20
        koma_position_score(19, 20) = 70
        koma_position_score(18, 11) = 10
        koma_position_score(18, 13) = 20
        koma_position_score(18, 21) = 40
        '飛車先の歩は突く
        koma_position_score(15, 34) = 1
        koma_position_score(15, 43) = 2
        '棒銀戦法
        koma_position_score(18, 15) = 10
        koma_position_score(18, 25) = 20
        koma_position_score(18, 34) = 30
        koma_position_score(18, 42) = 40
        koma_position_score(18, 43) = 40
        koma_position_score(18, 44) = 40
        'バイアスを先にかけておく
        For m = 0 To KOMA_KIND - 1 Step 1
            For n = 0 To KOMA_POS - 1 Step 1
                koma_position_score(m, n) = koma_position_score(m, n) * POSITION_BIAS / 100
            Next
        Next
        BoardSet()
        Randomize()
        'For i = 0 To score.Length - 1 Step 1
        'score(i) = score(i) + Rnd() * score(i) * 0.1
        'Next
        For i = 0 To KOMA_POS - 1 Step 1
            For j = 0 To 1
                Dim KomaColor As Integer = WHITE
                If j = 1 Then
                    KomaColor = BLACK
                End If
                CalcHuRange(i, KomaColor)
                CalcKeiRange(i, KomaColor)
                CalcKinRange(i, KomaColor)
                CalcGinRange(i, KomaColor)
                CalcOuRange(i, KomaColor)
                CalcKyoRange(i, KomaColor)
            Next
        Next
        LoadJyoseki()
        InitializeEval()
    End Sub
    Private Sub CalcHuRange(ByVal locate As Integer, ByVal wb As Integer)
        Dim x As Integer
        Dim y As Integer
        Dim dx As Integer
        Dim dy As Integer
        x = locate Mod 9
        y = Int(locate / 9)
        dx = x
        dy = y - 1 * wb
        bb_kiki.AddHuRange(locate, wb, dx, dy)
    End Sub
    Private Sub CalcKeiRange(ByVal locate As Integer, ByVal wb As Integer)
        Dim x As Integer
        Dim y As Integer
        Dim dx As Integer
        Dim dy As Integer
        x = locate Mod 9
        y = Int(locate / 9)
        dx = x + 1
        dy = y - 2 * wb
        bb_kiki.AddKeiRange(locate, wb, dx, dy)
        dx = x - 1
        dy = y - 2 * wb
        bb_kiki.AddKeiRange(locate, wb, dx, dy)
    End Sub
    Private Sub CalcKinRange(ByVal locate As Integer, ByVal wb As Integer)
        Dim x = locate Mod 9
        Dim y = Int(locate / 9)
        Dim dx = x
        Dim dy = y - 1
        bb_kiki.AddKinRange(locate, wb, dx, dy)
        dx = x + 1
        dy = y - 1 * wb
        bb_kiki.AddKinRange(locate, wb, dx, dy)
        dx = x - 1
        dy = y - 1 * wb
        bb_kiki.AddKinRange(locate, wb, dx, dy)
        dx = x + 1
        dy = y
        bb_kiki.AddKinRange(locate, wb, dx, dy)
        dx = x - 1
        dy = y
        bb_kiki.AddKinRange(locate, wb, dx, dy)
        dx = x
        dy = y + 1
        bb_kiki.AddKinRange(locate, wb, dx, dy)
    End Sub
    Private Sub CalcGinRange(ByVal locate As Integer, ByVal wb As Integer)
        Dim x As Integer
        Dim y As Integer
        Dim dx As Integer
        Dim dy As Integer
        x = locate Mod 9
        y = Int(locate / 9)
        dx = x
        dy = y - 1 * wb
        bb_kiki.AddGinRange(locate, wb, dx, dy)
        dx = x + 1
        dy = y - 1
        bb_kiki.AddGinRange(locate, wb, dx, dy)
        dx = x - 1
        dy = y - 1
        bb_kiki.AddGinRange(locate, wb, dx, dy)
        dx = x + 1
        dy = y + 1
        bb_kiki.AddGinRange(locate, wb, dx, dy)
        dx = x - 1
        dy = y + 1
        bb_kiki.AddGinRange(locate, wb, dx, dy)
    End Sub
    Private Sub CalcOuRange(ByVal locate As Integer, ByVal wb As Integer)
        Dim x As Integer
        Dim y As Integer
        Dim dx As Integer
        Dim dy As Integer
        x = locate Mod 9
        y = Int(locate / 9)
        dx = x
        dy = y - 1
        bb_kiki.AddOuRange(locate, wb, dx, dy)
        dx = x + 1
        dy = y - 1
        bb_kiki.AddOuRange(locate, wb, dx, dy)
        dx = x - 1
        dy = y - 1
        bb_kiki.AddOuRange(locate, wb, dx, dy)
        dx = x + 1
        dy = y + 1
        bb_kiki.AddOuRange(locate, wb, dx, dy)
        dx = x - 1
        dy = y + 1
        bb_kiki.AddOuRange(locate, wb, dx, dy)
        dx = x + 1
        dy = y
        bb_kiki.AddOuRange(locate, wb, dx, dy)
        dx = x - 1
        dy = y
        bb_kiki.AddOuRange(locate, wb, dx, dy)
        dx = x
        dy = y + 1
        bb_kiki.AddOuRange(locate, wb, dx, dy)
    End Sub
    Private Sub CalcKyoRange(ByVal locate As Integer, ByVal wb As Integer)
        Dim x As Integer
        Dim y As Integer
        Dim dx As Integer
        Dim dy As Integer
        Dim i As Integer
        x = locate Mod 9
        y = Int(locate / 9)
        For i = 1 To 8 Step 1
            dx = x
            dy = y - i * wb
            bb_kiki.AddKyoRange(locate, wb, dx, dy)
        Next
    End Sub
    Private Sub KomaKikiInit()
        komakiki_w = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}
        komakiki_b = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}
    End Sub
    Private Sub BoardSet()
        Dim p As Integer
        For p = 0 To 80 Step 1
            UnitSet(p)
        Next
    End Sub
    Private Sub UnitSet(ByVal locale As Integer)
        Dim b As Button
        Dim c As Integer
        b = GetButton(locale)
        c = board(locale)
        If c = 0 Then
            b.Text = ""
            b.BackColor = Color.BurlyWood
        ElseIf 1 <= c And c <= 14 Then
            b.Text = komaname(c)
            b.BackColor = Color.LemonChiffon
            b.ForeColor = Color.Black
        Else
            b.Text = komaname(c - 14)
            b.BackColor = Color.Cornsilk
            b.ForeColor = Color.Black
        End If
    End Sub
    Dim InitGenerate As Boolean = True
    Private Function UnitRange(ByVal locate As Integer) As List(Of Integer)
        Dim id As Integer = FindID(locate)
        If id = DUMMY_ID Then
            id = FromKind(board(locate), locate)
        End If
        Dim unit As Integer
        locate = locate
        unit = board(locate)
        UnitRange = New List(Of Integer)
        If id = DUMMY_ID Then
            Return UnitRange
        End If
        If 0 <= id And id < 40 Then
            KomaIDNode(id).Clear()
        End If
        Select Case unit
            Case 1
                UnitRange = HuRange(id, locate, 1)
            Case 2
                UnitRange = KyoRange(id, locate, 1)
            Case 3
                UnitRange = KeimaRange(id, locate, 1)
            Case 4
                UnitRange = GinRange(id, locate, 1)
            Case 5, 9, 10, 11, 12
                UnitRange = KinRange(id, locate, 1)
            Case 6
                UnitRange = HisyaRange(id, locate, 1, False)
            Case 7
                UnitRange = KakuRange(id, locate, 1, False)
            Case 8
                UnitRange = OuRange(id, locate, 1)
            Case 13
                UnitRange = HisyaRange(id, locate, 1, True)
            Case 14
                UnitRange = KakuRange(id, locate, 1, True)
            Case 15
                UnitRange = HuRange(id, locate, -1)
            Case 16
                UnitRange = KyoRange(id, locate, -1)
            Case 17
                UnitRange = KeimaRange(id, locate, -1)
            Case 18
                UnitRange = GinRange(id, locate, -1)
            Case 19, 23, 24, 25, 26
                UnitRange = KinRange(id, locate, -1)
            Case 20
                UnitRange = HisyaRange(id, locate, -1, False)
            Case 21
                UnitRange = KakuRange(id, locate, -1, False)
            Case 22
                UnitRange = OuRange(id, locate, -1)
            Case 27
                UnitRange = HisyaRange(id, locate, -1, True)
            Case 28
                UnitRange = KakuRange(id, locate, -1, True)
        End Select
        If 0 <= id And id < 40 Then
            If KILLER_SORT Then
                Dim last = KomaIDNode(id).Count - 1
                For i = 0 To last
                    Dim m As MoveData = KomaIDNode(id).ElementAt(i)
                    If m.capture <> BLANK Then
                        Node.Add(m)
                        KomaIDNode(id).RemoveAt(i)
                        i = i - 1
                        last = KomaIDNode(id).Count - 1
                    End If
                Next
            End If
            Node.InsertRange(NodeIdx, KomaIDNode(id))
            NodeIdx += KomaIDNode(id).Count
        End If
        If 0 < unit And unit <= 26 And unit <> 2 And unit <> 6 And unit <> 7 And unit <> 13 And unit <> 14 And unit <> 16 And unit <> 20 And unit <> 21 Then
            Dim dist = KikiBlocked.GetFirst()
            While dist <> -1
                Dim result = KomaBlocked(dist).Find(Function(n) n = id)
                If result < 1 Then
                    KomaBlocked(dist).Add(id)
                End If
                dist = KikiBlocked.GetNext()
            End While
        End If
    End Function

    Public Sub OrderMoves(moves As List(Of MoveData), ttMove As MoveData)
        Dim Move As MoveData
        For Each Move In moves
            ' ハッシュムーブ
            If False And ttMove IsNot Nothing AndAlso Move.from = ttMove.from AndAlso Move._to = ttMove._to Then
                Move.score = Integer.MaxValue
                ' キャプチャムーブ（MVV/LVA）
            ElseIf Move.capture <> BLANK Then
                Dim victimValue As Integer = GetPieceValue(Move.capture) ' 例：飛車=500, 歩=100
                Dim attackerValue As Integer = GetPieceValue(Move.from) ' 攻撃駒の価値
                Move.score = 10000 + victimValue - attackerValue
                ' キラームーブ
            ElseIf IsKillerMove(Move) Then
                Move.score = 9000
                ' プロモーション
            ElseIf Move.classup Then
                Move.score = 8000
                ' ヒストリー
            Else
                Move.score = GetHistoryScore(Move.from, Move._to)
            End If
        Next
        ' スコアで降順ソート
        moves.Sort(Function(a, b) b.score.CompareTo(a.score))
    End Sub

    Private Function GetPieceValue(piece As Integer) As Integer
        Return KomaScore(piece)
    End Function

    Private Function IsKillerMove(move As MoveData) As Boolean
        ' キラームーブテーブルをチェック
        Return (move.capture <> BLANK)
    End Function

    Private Function GetHistoryScore(fromSquare As Integer, toSquare As Integer) As Integer
        ' ヒストリーテーブルをチェック
        Return 0 ' 仮実装
    End Function
    Public Class TranspositionTableEntry
        Public Hash As ULong ' 局面のZobristハッシュ
        Public Depth As Integer = YOMI_DEPTH ' 探索深さ
        Public Score As Integer ' 評価値
        Public BestMove As MoveData ' 最善の指し手（ttMove）
        Public NodeType As Integer ' ノード種別（0=Exact, 1=Upper, 2=Lower）
    End Class

    Public Class TranspositionTable
        Private Table As Dictionary(Of ULong, TranspositionTableEntry)
        Public Sub New()
            Table = New Dictionary(Of ULong, TranspositionTableEntry)()
        End Sub

        Public Sub Store(hash As ULong, depth As Integer, score As Integer, bestMove As MoveData, nodeType As Integer)
            Dim entry As New TranspositionTableEntry With {
            .Hash = hash,
            .Depth = depth,
            .Score = score,
            .BestMove = bestMove,
            .NodeType = nodeType
        }
            Table(hash) = entry ' 簡略化：実際は衝突処理や上書きルールが必要
        End Sub

        Public Function Lookup(hash As ULong) As TranspositionTableEntry
            If Table.ContainsKey(hash) Then
                Return Table(hash)
            End If
            Return Nothing
        End Function
    End Class
    Private Function RangeCheck(ByRef r As Array, ByVal locate As Integer) As Boolean
        Dim i As Integer
        For i = 0 To r.Length - 1 Step 1
            If r(i) = locate Then
                RangeCheck = True
                Exit Function
            End If
        Next
        If robomode = False Then
            RangeCheck = False
        Else
            RangeCheck = False
        End If
    End Function
    Private Sub AddKomakiki(ByVal dx As Integer, ByVal dy As Integer)
        If KOMAKIKI_READ = True Then
            Dim dist As Integer
            If CheckBoardRange(dx, dy) = True Then
                dist = dx + dy * 9
                If IsWhite(undo) Then
                    komakiki_w(dist) += 1
                    Exit Sub
                End If
                If IsBlack(undo) Then
                    komakiki_b(dist) += 1
                    Exit Sub
                End If
            End If
        End If
    End Sub
    Private Sub AddRange(ByVal id As Byte, ByVal locate As Integer, ByVal dx As Integer, ByVal dy As Integer, ByRef list As List(Of Integer), ByVal pos As Integer, ByVal wb As Integer)
        Dim dist As Integer
        If CheckBoardRange(dx, dy) = True Then
            dist = dx + dy * 9
            If JigomaCheck(locate, dist) Then
                AddValue(id, locate, list, dist, pos, wb)
            End If
        End If
    End Sub
    Private Function JigomaCheck(ByVal locate As Integer, ByVal dist As Integer) As Boolean
        JigomaCheck = True
        If IsWhite(locate) Then
            If IsWhite(dist) Then
                JigomaCheck = False
            End If
            If KOMAKIKI_READ = True Then
                komakiki_w(dist) += 1
            End If
        End If
        If IsBlack(locate) Then
            If IsBlack(dist) Then
                JigomaCheck = False
            End If
            If KOMAKIKI_READ = True Then
                komakiki_b(dist) += 1
            End If
        End If
        Return JigomaCheck
    End Function
    Private Sub AddValue(ByVal id As Byte, ByVal locate As Integer, ByRef l As List(Of Integer), ByVal dist As Integer, ByVal pos As Integer, ByVal wb As Integer)
        l.Add(dist)
        If GenerationFlag = True Then
            KomaIDNode(id).Add(New MoveData(id, locate, dist, BLANK, wb))
            'If NARAZU_READ And (16 <= board(locate)) And (board(locate) <= 18) And ((locate >= 54) Or (dist >= 54)) Then
            'KomaIDNode(id).Add(New MoveData(id, locate, dist, BLANK, wb, False))
            'End If
        End If
    End Sub

    Private Function GetWB_BB(ByVal wb As Integer) As BitBoard
        If wb = WHITE Then
            Return bb_white
        Else
            Return bb_black
        End If
    End Function
    Private Function HuRange(ByVal id As Byte, ByVal locate As Integer, ByVal wb As Integer) As List(Of Integer)
        HuRange = New List(Of Integer)
        Dim dist As Integer = bb_kiki.GetHuRange(locate, wb, GetWB_BB(wb), KikiBlocked)
        If dist <> BLANK Then
            AddValue(id, locate, HuRange, dist, 0, wb)
        End If
        bb_kiki.GetHuKiki(locate, wb, bb_white, bb_black, komakiki_w, komakiki_b)
    End Function
    Private Function KyoRange(ByVal id As Byte, ByVal locate As Integer, ByVal wb As Integer) As List(Of Integer)
        KyoRange = New List(Of Integer)
        Dim bb As BitBoard = bb_kiki.GetKyoRange(locate, wb, bb_white, bb_black)
        Dim dist = bb.GetFirst()
        If dist = -1 Then
            dist = BLANK
        End If
        Dim idx = 0
        While dist <> BLANK
            AddValue(id, locate, KyoRange, dist, idx, wb)
            dist = bb.GetNext()
            If dist = -1 Then
                dist = BLANK
            End If
            idx += 1
        End While
        bb_kiki.GetKyoKiki(locate, wb, bb_white, bb_black, komakiki_w, komakiki_b)
    End Function
    Private Function AB(ByVal wb As Integer, ByVal locate As Integer, ByVal dist As Integer)
        AB = False
        If IsWB(wb, locate) And IsWB(-wb, dist) Then
            AB = True
        End If
        Return AB
    End Function
    Private Function HisyaRange(ByVal id As Byte, ByVal locate As Integer, ByVal wb As Integer, ByVal c As Boolean) As List(Of Integer)
        HisyaRange = New List(Of Integer)
        Dim x As Integer
        Dim y As Integer
        Dim dx As Integer
        Dim dy As Integer
        Dim i As Integer
        Dim dist As Integer
        x = locate Mod 9
        y = Int(locate / 9)
        For i = 1 To 8 Step 1
            dx = x
            dy = y - i * wb
            If CheckBoardRange(dx, dy) = True Then
                dist = dx + dy * 9
                If JigomaCheck(locate, dist) = False Then
                    AddKomakiki(dx, dy)
                    Exit For
                End If
                AddValue(id, locate, HisyaRange, dist, i, wb)
                If AB(wb, locate, dist) Then
                    KomaBlocked(dist).Add(id)
                    Exit For
                End If
            Else
                Exit For
            End If
        Next
        For i = 1 To 8 Step 1
            dx = x
            dy = y + i * wb
            If CheckBoardRange(dx, dy) = True Then
                dist = dx + dy * 9
                If JigomaCheck(locate, dist) = False Then
                    AddKomakiki(dx, dy)
                    Exit For
                End If
                AddValue(id, locate, HisyaRange, dist, i + 7, wb)
                If AB(wb, locate, dist) Then
                    KomaBlocked(dist).Add(id)
                    Exit For
                End If
            Else
                Exit For
            End If
        Next
        For i = 1 To 8 Step 1
            dx = x - i * wb
            dy = y
            If CheckBoardRange(dx, dy) = True Then
                dist = dx + dy * 9
                If JigomaCheck(locate, dist) = False Then
                    AddKomakiki(dx, dy)
                    Exit For
                End If
                AddValue(id, locate, HisyaRange, dist, i + 15, wb)
                If AB(wb, locate, dist) Then
                    KomaBlocked(dist).Add(id)
                    Exit For
                End If
            Else
                Exit For
            End If
        Next
        For i = 1 To 8 Step 1
            dx = x + i * wb
            dy = y
            If CheckBoardRange(dx, dy) = True Then
                dist = dx + dy * 9
                If JigomaCheck(locate, dist) = False Then
                    AddKomakiki(dx, dy)
                    Exit For
                End If
                AddValue(id, locate, HisyaRange, dist, i + 23, wb)
                If AB(wb, locate, dist) Then
                    KomaBlocked(dist).Add(id)
                    Exit For
                End If
            Else
                Exit For
            End If
        Next
        If c = True Then
            dx = x - 1
            dy = y - 1
            AddRange(id, locate, dx, dy, HisyaRange, 32, wb)
            dx = x + 1
            dy = y + 1
            AddRange(id, locate, dx, dy, HisyaRange, 33, wb)
            dx = x - 1
            dy = y + 1
            AddRange(id, locate, dx, dy, HisyaRange, 34, wb)
            dx = x + 1
            dy = y - 1
            AddRange(id, locate, dx, dy, HisyaRange, 35, wb)
        End If
    End Function
    Private Function KakuRange(ByVal id As Byte, ByVal locate As Integer, ByVal wb As Integer, ByVal c As Boolean) As List(Of Integer)
        KakuRange = New List(Of Integer)
        Dim x As Integer
        Dim y As Integer
        Dim dx As Integer
        Dim dy As Integer
        Dim i As Integer
        Dim dist As Integer
        x = locate Mod 9
        y = Int(locate / 9)
        For i = 1 To 8 Step 1
            dx = x - i
            dy = y - i
            If CheckBoardRange(dx, dy) = True Then
                dist = dx + dy * 9
                If JigomaCheck(locate, dist) = False Then
                    KomaBlocked(dist).Add(id)
                    Exit For
                End If
                If AB(wb, locate, dist) Then
                    AddValue(id, locate, KakuRange, dist, i, wb)
                    KomaBlocked(dist).Add(id)
                    Exit For
                Else
                    AddValue(id, locate, KakuRange, dist, i, wb)
                End If
            Else
                Exit For
            End If
        Next
        For i = 1 To 8 Step 1
            dx = x + i
            dy = y + i
            If CheckBoardRange(dx, dy) = True Then
                dist = dx + dy * 9
                If JigomaCheck(locate, dist) = False Then
                    KomaBlocked(dist).Add(id)
                    Exit For
                End If
                If AB(wb, locate, dist) Then
                    AddValue(id, locate, KakuRange, dist, i + 7, wb)
                    KomaBlocked(dist).Add(id)
                    Exit For
                Else
                    AddValue(id, locate, KakuRange, dist, i + 7, wb)
                End If
            Else
                Exit For
            End If
        Next
        For i = 1 To 8 Step 1
            dx = x - i
            dy = y + i
            If CheckBoardRange(dx, dy) = True Then
                dist = dx + dy * 9
                If JigomaCheck(locate, dist) = False Then
                    KomaBlocked(dist).Add(id)
                    Exit For
                End If
                If AB(wb, locate, dist) Then
                    AddValue(id, locate, KakuRange, dist, i + 15, wb)
                    KomaBlocked(dist).Add(id)
                    Exit For
                Else
                    AddValue(id, locate, KakuRange, dist, i + 15, wb)
                End If
            Else
                Exit For
            End If
        Next
        For i = 1 To 8 Step 1
            dx = x + i
            dy = y - i
            If CheckBoardRange(dx, dy) = True Then
                dist = dx + dy * 9
                If JigomaCheck(locate, dist) = False Then
                    KomaBlocked(dist).Add(id)
                    Exit For
                End If
                If AB(wb, locate, dist) Then
                    AddValue(id, locate, KakuRange, dist, i + 23, wb)
                    KomaBlocked(dist).Add(id)
                    Exit For
                Else
                    AddValue(id, locate, KakuRange, dist, i + 23, wb)
                End If
            Else
                Exit For
            End If
        Next
        If c = True Then
            dx = x
            dy = y - 1
            AddRange(id, locate, dx, dy, KakuRange, 32, wb)
            dx = x
            dy = y + 1
            AddRange(id, locate, dx, dy, KakuRange, 33, wb)
            dx = x - 1
            dy = y
            AddRange(id, locate, dx, dy, KakuRange, 34, wb)
            dx = x + 1
            dy = y
            AddRange(id, locate, dx, dy, KakuRange, 35, wb)
        End If
    End Function
    Private Function KeimaRange(ByVal id As Byte, ByVal locate As Integer, ByVal wb As Integer) As List(Of Integer)
        KeimaRange = New List(Of Integer)
        Dim bb As BitBoard = bb_kiki.GetKeiRange(locate, wb, GetWB_BB(wb), KikiBlocked)
        Dim dist = bb.GetFirst()
        If dist = -1 Then
            dist = BLANK
        End If
        Dim idx = 0
        While dist <> BLANK
            AddValue(id, locate, KeimaRange, dist, idx, wb)
            dist = bb.GetNext()
            If dist = -1 Then
                dist = BLANK
            End If
            idx += 1
        End While
        bb_kiki.GetKeiKiki(locate, wb, bb_white, bb_black, komakiki_w, komakiki_b)
    End Function
    Private Function GinRange(ByVal id As Byte, ByVal locate As Integer, ByVal wb As Integer) As List(Of Integer)
        GinRange = New List(Of Integer)
        Dim bb As BitBoard = bb_kiki.GetGinRange(locate, wb, GetWB_BB(wb), KikiBlocked)
        Dim dist = bb.GetFirst()
        If dist = -1 Then
            dist = BLANK
        End If
        Dim idx = 0
        While dist <> BLANK
            AddValue(id, locate, GinRange, dist, idx, wb)
            dist = bb.GetNext()
            If dist = -1 Then
                dist = BLANK
            End If
            idx += 1
        End While
        bb_kiki.GetGinKiki(locate, wb, bb_white, bb_black, komakiki_w, komakiki_b)
    End Function
    Private Function KinRange(ByVal id As Byte, ByVal locate As Integer, ByVal wb As Integer) As List(Of Integer)
        KinRange = New List(Of Integer)
        Dim bb As BitBoard = bb_kiki.GetKinRange(locate, wb, GetWB_BB(wb), KikiBlocked)
        Dim dist = bb.GetFirst()
        If dist = -1 Then
            dist = BLANK
        End If
        Dim idx = 0
        While dist <> BLANK
            AddValue(id, locate, KinRange, dist, idx, wb)
            dist = bb.GetNext()
            If dist = -1 Then
                dist = BLANK
            End If
            idx += 1
        End While
        bb_kiki.GetKinKiki(locate, wb, bb_white, bb_black, komakiki_w, komakiki_b)
    End Function
    Private Function OuRange(ByVal id As Byte, ByVal locate As Integer, ByVal wb As Integer) As List(Of Integer)
        OuRange = New List(Of Integer)
        Dim bb As BitBoard = bb_kiki.GetOuRange(locate, wb, GetWB_BB(wb), KikiBlocked)
        Dim dist = bb.GetFirst()
        If dist = -1 Then
            dist = BLANK
        End If
        Dim idx = 0
        While dist <> BLANK
            AddValue(id, locate, OuRange, dist, idx, wb)
            dist = bb.GetNext()
            If dist = -1 Then
                dist = BLANK
            End If
            idx += 1
        End While
        bb_kiki.GetOuKiki(locate, wb, bb_white, bb_black, komakiki_w, komakiki_b)
    End Function
    Private Function CheckBoardRange(ByVal x As Integer, ByVal y As Integer) As Boolean
        CheckBoardRange = True
        If x < 0 Or 8 < x Then
            CheckBoardRange = False
        End If
        If y < 0 Or 8 < y Then
            CheckBoardRange = False
        End If
    End Function
    Private Function IsWhite(ByVal locate As Integer) As Boolean
        If bb_white.IsExist(locate) > 0 Then
            IsWhite = True
            Return IsWhite
        End If
        Return False
    End Function
    Private Function IsBlack(ByVal locate As Integer) As Boolean
        If bb_black.IsExist(locate) > 0 Then
            IsBlack = True
            Return IsBlack
        End If
        Return False
    End Function
    Private Function IsWB(ByVal wb As Integer, ByVal i As Integer) As Boolean
        IsWB = False
        If wb = WHITE Then
            IsWB = IsWhite(i)
            Return IsWB
        ElseIf wb = BLACK Then
            IsWB = IsBlack(i)
            Return IsWB
        End If
        Return IsWB
    End Function
    Private Function IsEnemyKing(ByVal wb As Integer, ByVal i As Integer) As Boolean
        IsEnemyKing = False
        If wb = WHITE Then
            If board(i) = 22 Then
                IsEnemyKing = True
            End If
        Else
            If board(i) = 8 Then
                IsEnemyKing = True
            End If
        End If
    End Function
    Private Function max(ByVal a As Integer, ByVal b As Integer)
        If a >= b Then
            max = a
        Else
            max = b
        End If
        Return max
    End Function
    Private Function min(ByVal a As Integer, ByVal b As Integer)
        If a <= b Then
            min = a
        Else
            min = b
        End If
        Return min
    End Function
    Function ReverceWB(ByVal wb As Integer) As Integer
        If wb = -1 Then
            ReverceWB = 1
        Else
            ReverceWB = -1
        End If
    End Function
    Private Function KomaScore(ByVal koma) As Integer
        Static score As Array = {0, 90, 315, 405, 495, 540, 990, 855, 9999, 540, 540, 540, 540, 1395, 945, 90, 315, 405, 495, 540, 990, 855, 9999, 540, 540, 540, 540, 1395, 945}
        If True Then
            If koma < 0 Or score.Length <= koma Then
                Console.WriteLine("KomaScore IndexOver !")
                Return 0
            End If
        End If
        KomaScore = score(koma)
    End Function
    Private Function KomaDist(ByVal king_pos, ByVal koma) As Integer
        Dim king_x As Integer
        Dim king_y As Integer
        king_x = king_pos Mod 9
        king_y = king_pos / 9
        Dim x = king_x - (koma Mod 9)
        Dim y = king_y - koma / 9
        KomaDist = Math.Sqrt(x ^ 2 + y ^ 2) / 456
    End Function
    Private Function Hyouka() As Integer
        king_pos = -1
        enem_pos = -1
        Dim d As Integer = 0
        WBuf.Init()
        BBuf.Init()
        Hyouka = 0
        For i = 0 To 80 Step 1
            If board(i) = 0 Then
                Continue For
            End If
            If board(i) = 8 Then
                king_pos = i
            End If
            If board(i) = 22 Then
                enem_pos = i
            End If
        Next
        If enem_pos = -1 Then
            Return FINISH_SCORE
        End If
        If king_pos = -1 Then
            Return -FINISH_SCORE
        End If
        For i = 0 To 80 Step 1
            If board(i) = 0 Then
                WBuf.komakiki += score_table(komakiki_w(i), 2, king_pos, i) * 100
                BBuf.komakiki += score_table(komakiki_b(i), 2, enem_pos, i) * 100
                Continue For
            End If
            If IsWB(WHITE, i) Then
                WBuf.komatoku += KomaScore(board(i)) * 100
                WBuf.komaichi += koma_position_score(board(i), i) * 100
                ''WBuf.komakiki += score_table(komakiki_w(i), 0, enem_pos, i)
                BBuf.komakiki += score_table(komakiki_b(i), 1, enem_pos, i) * 100
            End If
            If IsWB(BLACK, i) Then
                BBuf.komatoku += KomaScore(board(i)) * 100
                BBuf.komaichi += koma_position_score(board(i), i) * 100
                WBuf.komakiki += score_table(komakiki_w(i), 1, king_pos, i) * 100
                'BBuf.komakiki += score_table(komakiki_b(i), 0, king_pos, i)
            End If
        Next
        For i = 0 To 7 Step 1
            'WBuf.komatoku += tegomaw(i) * KomaScore(KomaIdx(i, WHITE)) * 1.05
            'BBuf.komatoku += tegomab(i) * KomaScore(KomaIdx(i, BLACK)) * 1.05
        Next
        For i = 0 To tegomaw.Count - 1 Step 1
            WBuf.komatoku += KomaScore(Piece(tegomaw(i)).omote) * 105
        Next
        For i = 0 To tegomab.Count - 1 Step 1
            BBuf.komatoku += KomaScore(Piece(tegomab(i)).omote) * 105
        Next
        Hyouka += WBuf.komatoku
        Hyouka += WBuf.komaichi
        Hyouka += WBuf.komakiki
        Hyouka -= BBuf.komatoku
        Hyouka -= BBuf.komaichi
        Hyouka -= BBuf.komakiki
        Hyouka = Hyouka / 2
        Return Hyouka / 100
    End Function
    Private Function IsKillerMove(ByVal wb As Integer, ByVal dst As Integer) As Boolean
        Return IsWB(-wb, dst)
    End Function
    Public Class Zobrist
        ' Zobristテーブル
        Public Table As ULong(,,) ' [マス][駒種][プレイヤー]
        Public Turn As ULong() ' [プレイヤー]
        Public Hand As ULong(,,) ' [駒種][プレイヤー][枚数]

        ' 初期化
        Public Sub Initialize()
            Dim rand As New Random(42) ' 再現性のためシード固定
            Table = New ULong(80, 14, 1) {} ' 81マス、15駒種（0=空）、2プレイヤー（先手=0, 後手=1）
            Turn = New ULong(1) {} ' 先手=0, 後手=1
            Hand = New ULong(6, 1, 18) {} ' 7駒種（歩,桂,香,銀,金,角,飛）、2プレイヤー、最大18枚

            ' 盤面のZobrist値
            For square As Integer = 0 To 80
                For piece As Integer = 0 To 14 ' 0=空, 1=歩, ..., 14=龍
                    For player As Integer = 0 To 1
                        Table(square, piece, player) = GenerateRandomULong(rand)
                    Next
                Next
            Next

            ' 手番
            For player As Integer = 0 To 1
                Turn(player) = GenerateRandomULong(rand)
            Next

            ' 持ち駒
            For piece As Integer = 0 To 6 ' 歩,桂,香,銀,金,角,飛
                For player As Integer = 0 To 1
                    For count As Integer = 0 To 18
                        Hand(piece, player, count) = GenerateRandomULong(rand)
                    Next
                Next
            Next
        End Sub

        Private Function GenerateRandomULong(rand As Random) As ULong
            Dim bytes(7) As Byte
            rand.NextBytes(bytes)
            Return BitConverter.ToUInt64(bytes, 0)
        End Function
    End Class

    Public Class Position
        Private Board As Integer() ' 盤面：マスごとの駒（例：1=先手歩, -1=後手歩, 0=空）
        Private TurnPlayer As Integer ' 0=先手, 1=後手
        Private Hand As Integer(,) ' 持ち駒：[駒種][プレイヤー]
        Private CurrentHash As ULong ' 現在のZobristハッシュ
        Public _Zobrist As Zobrist

        Public Sub New()
            Board = New Integer(80) {} ' 81マス
            TurnPlayer = 0
            Hand = New Integer(6, 1) {} ' 7駒種×2プレイヤー
            CurrentHash = 0
            _Zobrist = New Zobrist
            _Zobrist.Initialize() ' Zobristテーブル初期化
            UpdateHash() ' 初期局面のハッシュを計算
        End Sub

        ' Zobristハッシュを計算（初回または再計算用）
        Private Sub UpdateHash()
            CurrentHash = 0
            ' 盤面
            For square As Integer = 0 To 80
                Dim piece As Integer = Board(square)
                If piece <> 0 Then
                    Dim pieceType As Integer = Math.Abs(piece) ' 駒種（1=歩, ..., 14=龍）
                    Dim player As Integer = If(piece > 0, 0, 1) ' 先手=0, 後手=1
                    CurrentHash = CurrentHash Xor _Zobrist.Table(square, pieceType, player)
                End If
            Next
            ' 手番
            CurrentHash = CurrentHash Xor _Zobrist.Turn(TurnPlayer)
            ' 持ち駒
            For piece As Integer = 0 To 6
                For player As Integer = 0 To 1
                    Dim count As Integer = Hand(piece, player)
                    If count > 0 Then
                        CurrentHash = CurrentHash Xor _Zobrist.Hand(piece, player, count)
                    End If
                Next
            Next
        End Sub

        ' Zobristハッシュを取得
        Public Function GetZobristHash() As ULong
            Return CurrentHash
        End Function
        Private Function ClassUp(ByVal unit As Integer) As Integer
            Dim unit_up = unit
            Select Case unit
                Case 1, 2, 3, 4
                    unit_up = unit + 8
                Case 6, 7
                    unit_up = unit + 7
                Case 15, 16, 17, 18
                    unit_up = unit + 8
                Case 20, 21
                    unit_up = unit + 7
            End Select
            Return unit_up
        End Function
        Private Function ClassDown(ByVal kind As Integer) As Integer
            Dim unit As Integer = kind
            Select Case unit
                Case 9, 10, 11, 12
                    unit = unit - 8
                Case 13, 14
                    unit = unit - 7
                Case 23, 24, 25, 26
                    unit = unit - 8
                Case 27, 28
                    unit = unit - 7
            End Select
            Return unit
        End Function

        ' 指し手を適用（ハッシュを増分更新）
        Public Sub MakeMove(move As MoveData)
            Dim fromSquare As Integer = move.from
            Dim toSquare As Integer = move._to
            Dim piece As Integer = Board(fromSquare)
            Dim captured As Integer = Board(toSquare)
            Dim pieceType As Integer = Math.Abs(piece)
            Dim player As Integer = If(piece > 0, 0, 1)

            ' 1. 移動元を空に
            CurrentHash = CurrentHash Xor _Zobrist.Table(fromSquare, pieceType, player)
            ' 2. 移動先に駒を配置（成り考慮）
            Dim newPieceType As Integer = If(move.classup, ClassUp(pieceType), pieceType) ' 例：歩(1)→と金(8)
            CurrentHash = CurrentHash Xor _Zobrist.Table(toSquare, newPieceType, player)
            ' 3. 駒を取った場合
            If captured <> 0 Then
                Dim capturedType As Integer = Math.Abs(captured)
                Dim capturedPlayer As Integer = If(captured <> BLANK, 0, 1)
                CurrentHash = CurrentHash Xor _Zobrist.Table(toSquare, capturedType, capturedPlayer)
                ' 持ち駒に追加
                Dim handPiece As Integer = If(capturedType > 7, ClassDown(capturedType), capturedType) ' 例：と金(8)→歩(1)
                Dim oldCount As Integer = Hand(handPiece, player)
                CurrentHash = CurrentHash Xor _Zobrist.Hand(handPiece, player, oldCount)
                Hand(handPiece, player) += 1
                CurrentHash = CurrentHash Xor _Zobrist.Hand(handPiece, player, oldCount + 1)
            End If
            ' 4. 手番を変更
            CurrentHash = CurrentHash Xor _Zobrist.Turn(TurnPlayer)
            TurnPlayer = 1 - TurnPlayer
            CurrentHash = CurrentHash Xor _Zobrist.Turn(TurnPlayer)

            ' 盤面更新
            Board(toSquare) = If(move.classup, ClassUp(pieceType), piece)
            Board(fromSquare) = 0
        End Sub

        ' 指し手を戻す（ハッシュも復元）
        Public Sub UnmakeMove(move As MoveData)
            Dim fromSquare As Integer = move.from
            Dim toSquare As Integer = move._to
            Dim piece As Integer = Board(toSquare)
            Dim captured As Integer = move.capture
            Dim pieceType As Integer = Math.Abs(piece)
            Dim player As Integer = If(piece > 0, 0, 1)

            ' 1. 手番を戻す
            CurrentHash = CurrentHash Xor _Zobrist.Turn(TurnPlayer)
            TurnPlayer = 1 - TurnPlayer
            CurrentHash = CurrentHash Xor _Zobrist.Turn(TurnPlayer)
            ' 2. 移動先を元に戻す（駒取り考慮）
            If captured <> 0 Then
                Dim capturedType As Integer = Math.Abs(captured)
                Dim capturedPlayer As Integer = If(captured > 0, 0, 1)
                CurrentHash = CurrentHash Xor _Zobrist.Table(toSquare, pieceType, player)
                CurrentHash = CurrentHash Xor _Zobrist.Table(toSquare, capturedType, capturedPlayer)
                ' 持ち駒を減らす
                Dim handPiece As Integer = If(capturedType > 7, ClassDown(capturedType), capturedType)
                Dim oldCount As Integer = Hand(handPiece, player)
                CurrentHash = CurrentHash Xor _Zobrist.Hand(handPiece, player, oldCount)
                Hand(handPiece, player) -= 1
                CurrentHash = CurrentHash Xor _Zobrist.Hand(handPiece, player, oldCount - 1)
            Else
                CurrentHash = CurrentHash Xor _Zobrist.Table(toSquare, pieceType, player)
            End If
            ' 3. 移動元に駒を戻す
            Dim origPieceType As Integer = If(move.classup, ClassDown(pieceType), pieceType)
            CurrentHash = CurrentHash Xor _Zobrist.Table(fromSquare, origPieceType, player)

            ' 盤面復元
            Board(fromSquare) = If(piece > 0, origPieceType, -origPieceType)
            Board(toSquare) = captured
        End Sub
    End Class
    Private Function alphabeta(ByVal position As Position, ByVal first As Integer, ByVal wb As Integer, ByVal depth As Integer,
                                ByVal alpha As Integer, ByVal beta As Integer, tt As TranspositionTable) As Integer
        'Dim h As Integer = Hyouka() * wb
        Dim h As Integer = (currentEval / 200) * wb
        If depth = 0 Then
            Return h
        End If
        If Math.Abs(h) >= FINISH_SCORE Then
            Return h
        End If
        ' 置換表を参照
        Dim hash As ULong = position.GetZobristHash()
        Dim ttEntry As TranspositionTableEntry = tt.Lookup(hash)
        Dim ttMove As MoveData = Nothing
        If USE_HASH And ttEntry IsNot Nothing AndAlso ttEntry.Depth >= depth Then
            ttMove = ttEntry.BestMove
            ' 評価値が使える場合、探索をスキップ
            If ttEntry.NodeType = 0 Then ' Exact
                Return ttEntry.Score
            ElseIf ttEntry.NodeType = 1 AndAlso ttEntry.Score <= alpha Then ' Upper
                Return alpha
            ElseIf ttEntry.NodeType = 2 AndAlso ttEntry.Score >= beta Then ' Lower
                Return beta
            End If
        End If
        Dim last As Integer = GenerateMoves(first, wb, depth)
        ' 指し手オーダリング
        OrderMoves(Node.GetRange(first, last - first), ttMove)
        For i = first To last - 1 Step 1
            MakeMove(Node(i), False)
            Dim a = -alphabeta(position, last, -wb, depth - 1, -beta, -alpha, tt)
            UnmakeMove()
            If (a > alpha) Then
                alpha = a
                If depth = YOMI_DEPTH Then
                    best = Node(i)
                    best.eval = h
                    best.best_eval = alpha
                    best.read_depth = YOMI_DEPTH
                    BestScore = alpha
                    WTop = WBuf
                    BTop = BBuf
                End If
            End If
            If alpha >= beta Then
                Exit For
            End If
        Next
        If USE_HASH Then
            ' 置換表に保存
            Dim nodeType As Integer = If(BestScore <= alpha, 1, If(BestScore >= beta, 2, 0))
            tt.Store(hash, depth, BestScore, best, nodeType)
        End If
        Return alpha
    End Function
    Private Function GenerateMoves(ByVal first As Integer, ByVal wb As Integer,
                                        ByVal depth As Integer) As Integer
        Dim idx As Integer = first
        If KOMAKIKI_READ Then
            KomaKikiInit()
        End If
        If NIRAMI_READ Then
            nirami_w = 0
            nirami_b = 0
        End If
        Dim bb As BitBoard = New BitBoard
        If wb = WHITE Then
            bb.b1 = bb_white.b1
            bb.b2 = bb_white.b2
        Else
            bb.b1 = bb_black.b1
            bb.b2 = bb_black.b2
        End If
        Dim pos = bb.GetFirst()
        While (pos <> -1)
            If True = IsWB(wb, pos) Then
                undo = pos
                GenerationFlag = True
                NodeIdx = idx
                UnitRange(pos)
                GenerationFlag = False
                idx += (NodeIdx - idx)
            End If
            pos = bb.GetNext()
        End While
        InitGenerate = False
        If False And HAND_READ And (depth > HAND_RIMIT) Then
            For i = 0 To 6 '手駒の玉は読まない
                If wb = WHITE Then
                    If tegomaw(i) > 0 Then
                        range = HandRange(wb, i).ToArray
                        For j = 0 To range.Length - 1 Step 1
                            Node(idx) = TegomaRange(i)(range(j))
                            Node(idx).teban = wb
                            idx += 1
                        Next
                    End If
                ElseIf tegomab(i) > 0 Then
                    range = HandRange(wb, i).ToArray
                    For j = 0 To range.Length - 1 Step 1
                        Node(idx) = TegomaRange(i + 15)(range(j))
                        Node(idx).teban = wb
                        idx += 1
                    Next
                End If
            Next
        End If
        Return idx
    End Function
    Private HashCount As Dictionary(Of String, Integer)
    Private Sub RobotMove(ByVal wb As Integer)
        If ROBOT_MOVE = False Then
            Exit Sub
        End If
        If modosi.Count = 0 Then
            HashCount = New Dictionary(Of String, Integer)
        End If
        Dim hash As String
        Dim c As Integer
        Dim from As Integer
        Dim _to As Integer
        Dim nodemax As Integer
        Dim nodemin As Integer
        c = 0
        'Dim starttime As Long = Now.Hour * 3600 + Now.Minute * 60 + Now.Second
        nodemax = 214748364
        nodemin = -214748364
        SuspendLayout()
        Dim ret As Integer = 0
        best.from = BLANK
        Dim position As New Position()
        Dim tt As New TranspositionTable()
        Dim s As String = _b.GetBoardString(board)
        If USE_JYOSEKI Then
            If _JyosekiDictionary.ContainsKey(s) Then
                best.SetMoveDataFromString(_JyosekiDictionary(s))
            End If
        Else
            ret = alphabeta(position, 0, wb, YOMI_DEPTH, nodemin, nodemax, tt)
            Dim s2 As String = best.GetMoveDataString()
            If True Then
                If Not _JyosekiDictionary.ContainsKey(s) Then
                    _JyosekiDictionary.Add(s, s2)
                End If
                For Each key In _JyosekiDictionary.Keys
                    If key = s Then
                        hash = key.GetHashCode()
                    End If
                Next
            End If
        End If
        If RETURN_LOG Then
            ListBox1.Items.Add(ret)
        End If
        robomode = True
        ResumeLayout()
        Dim s4 As String = best.GetMoveDataString()
        Dim s3 As String = s
        Dim BoardLoop As Boolean = False
        If HashCount.ContainsKey(s3) Then
            Dim count = HashCount(s3)
            HashCount(s3) += 1
            If HashCount(s3) >= 4 Then
                BoardLoop = True
            End If
        Else
            HashCount.Add(s3, 0)
        End If
        If BoardLoop Then
            ListBox1.Items.Add("▽千日手")
            ListBox1.TopIndex = ListBox1.Items.Count - 1
            robomode = False
        End If
        If BestScore <= -FINISH_SCORE Then
            ListBox1.Items.Add("▽投了")
            ListBox1.TopIndex = ListBox1.Items.Count - 1
            robomode = False
        ElseIf best.hand = BLANK Then
            from = best.from
            _to = best._to
            GetButton(from).PerformClick()
            GetButton(_to).PerformClick()
            robomode = False
        Else
            from = best.hand - 14
            _to = best._to
            GetHandBlack(from).PerformClick()
            GetButton(_to).PerformClick()
            robomode = False
        End If
        If BestScore >= FINISH_SCORE Then
            ListBox1.Items.Add("▲詰み")
            ListBox1.TopIndex = ListBox1.Items.Count - 1
            robomode = False
        End If
    End Sub
    Private Sub UnitClick(ByVal locate As Integer)
        locate = locate - 1
        Dim b As Button
        Dim c As Integer
        Dim r As Integer
        Dim d As MoveData = New MoveData
        Dim id As Integer
        r = False
        If state = ST_FREE Then
            undo = locate
            range = UnitRange(locate).ToArray
            For i = 0 To range.Length - 1 Step 1
                If range(i) <> BLANK Then
                    r = True
                End If
            Next
            If r = False Then
                Exit Sub
            End If
            If IsWhite(locate) Then
                state = ST_WHITE_CHOOSE
            ElseIf IsBlack(locate) Then
                state = ST_BLACK_CHOOSE
            Else
                Exit Sub
            End If
            For c = 0 To range.Length - 1 Step 1
                b = GetButton(range(c))
                If IsWhite(locate) = True Then
                    b.BackColor = Color.SandyBrown
                ElseIf IsBlack(locate) = True Then
                    b.BackColor = Color.DarkOrange
                Else
                    b.BackColor = Color.YellowGreen
                End If
                If REVERSE_BUTTON Then
                    Dim n = board(range(c))
                    If n = 0 Then
                        b.Text = ""
                    ElseIf 1 <= n And n <= 14 Then
                        b.Text = komaname(n)
                    Else
                        b.Text = komaname(n - 14)
                    End If
                    SetButtonImage(b)
                End If
            Next
        ElseIf state = ST_WHITE_CHOOSE And RangeCheck(range, locate) Then
            'MoveChara(locate)
            d.hand = BLANK
            id = FindID(undo)
            If id = DUMMY_ID Then
                id = FromKind(board(undo), undo)
            End If
            d.komaID = id
            d.from = undo
            d._to = locate
            d.teban = WHITE
            MakeMove(d, True)
            DispAll()
            AddKihu(locate)
            state = ST_FREE
            Me.Refresh()
            Me.Cursor = Cursors.WaitCursor
            RobotMove(-1)
            Me.Cursor = Cursors.Default
        ElseIf (state = ST_WHITE_CHOOSE Or state = ST_BLACK_CHOOSE) And undo = locate Then
            DispAll()
            state = 0
        ElseIf state = ST_BLACK_CHOOSE And RangeCheck(range, locate) Then
            'MoveChara(locate)
            d.hand = BLANK
            id = FindID(undo)
            If id = DUMMY_ID Then
                id = FromKind(board(undo), undo)
            End If
            d.komaID = id
            d.from = undo
            d._to = locate
            d.teban = BLACK
            MakeMove(d, True)
            DispAll()
            AddKihu(locate)
            state = ST_FREE
        ElseIf state = ST_WHITE_MOVE And RangeCheck(range, locate) Then
            'board(locate) = pop
            'tegomaw(pop - 1) = tegomaw(pop - 1) - 1
            d.hand = pop
            d.komaID = GetTegomaIDFromKind(pop, WHITE)
            d.from = undo
            d._to = locate
            d.teban = WHITE
            MakeMove(d, True)
            DispAll()
            AddKihu(locate)
            undo = BLANK
            komaundo = BLANK
            state = ST_FREE
            Me.Refresh()
            Me.Cursor = Cursors.WaitCursor
            RobotMove(-1)
            Me.Cursor = Cursors.Default
        ElseIf state = ST_BLACK_MOVE And RangeCheck(range, locate) Then
            'board(locate) = pop
            'tegomab(pop - 15) = tegomab(pop - 15) - 1
            d.hand = pop
            d.komaID = GetTegomaIDFromKind(pop, BLACK)
            d.from = undo
            d._to = locate
            d.teban = BLACK
            MakeMove(d, True)
            DispAll()
            AddKihu(locate)
            undo = BLANK
            komaundo = BLANK
            state = ST_FREE
        End If
    End Sub

    Const REVERSE_BUTTON = True
    Private Sub SetReverseImage()
        If REVERSE_BUTTON Then
            For i As Integer = 0 To 80
                Dim btn As Button = GetButton(i)
                If btn IsNot Nothing Then
                    SetButtonImage(btn)
                End If
            Next
        End If
    End Sub
    Private Sub DispAll()
        Me.SuspendLayout()
        For p = 0 To 80 Step 1
            UnitSet(p)
        Next
        DispHand()
        TextBox3.Text = Hyouka().ToString
        TextBox4.Text = -Hyouka().ToString
        SetReverseImage()
        Me.ResumeLayout()
    End Sub
    Private Sub DrawShogiBoard(ByVal panel As Panel)
        Dim bmp As New Bitmap(panel.Width, panel.Height)
        Using g As Graphics = Graphics.FromImage(bmp)
            g.Clear(panel.BackColor) ' 背景をクリア
            g.SmoothingMode = SmoothingMode.AntiAlias

            ' マス目を計算
            Dim cellWidth As Single = panel.Width / 9
            Dim cellHeight As Single = panel.Height / 9

            ' グリッド線を描画
            Using pen As New Pen(Color.Black, 1)
                ' 縦線
                For i As Integer = 0 To 8
                    Dim x As Single = i * cellWidth
                    g.DrawLine(pen, x, 0, x, panel.Height)
                    For j = 0 To 8
                        Dim pos = (8 - i) + j * 9
                        Dim btn As Button = GetButton(pos)
                        btn.Left = x + 3
                    Next

                Next
                ' 横線
                For i As Integer = 0 To 8
                    Dim y As Single = i * cellHeight
                    g.DrawLine(pen, 0, y, panel.Width, y)
                Next
            End Using
            For i As Integer = 3 To 8 Step 3
                For j As Integer = 3 To 8 Step 3
                    Dim x As Single = i * cellWidth - 3
                    Dim y As Single = j * cellHeight - 3
                    Dim r As Rectangle = New Rectangle(x, y, 6, 6)
                    g.DrawEllipse(Pens.Black, r)
                Next
            Next
        End Using
        panel.BackgroundImage = bmp


    End Sub

    Private Function HandRange(ByVal wb As Integer, ByVal idx As Integer) As List(Of Integer)
        'Dim koma = KomaIdx(idx, wb)
        Dim koma = ClassDown(GetTegoma(idx, wb))
        range = all.Clone()
        For i = 0 To 80 Step 1
            If board(i) <> 0 Then
                range(i) = BLANK
            End If
        Next
        If koma = 1 Then
            For i = 0 To 8 Step 1
                range(i) = BLANK
            Next
            For i = 0 To 8 Step 1
                For j = 0 To 8
                    If board(i + j * 9) = 1 Then
                        For k = 0 To 8
                            range(i + k * 9) = BLANK
                        Next
                    End If
                Next
            Next
        End If
        If koma = 2 Then
            For i = 0 To 8 Step 1
                range(i) = BLANK
            Next
        End If
        If koma = 3 Then
            For i = 0 To 17 Step 1
                range(i) = BLANK
            Next
        End If
        If koma = 15 Then
            For i = 72 To 80 Step 1
                range(i) = BLANK
            Next
            For i = 0 To 8 Step 1
                For j = 0 To 8
                    If board(i + j * 9) = 15 Then
                        For k = 0 To 8
                            range(i + k * 9) = BLANK
                        Next
                    End If
                Next
            Next
        End If
        If koma = 16 Then
            For i = 72 To 80 Step 1
                range(i) = BLANK
            Next
        End If
        If koma = 17 Then
            For i = 63 To 80 Step 1
                range(i) = BLANK
            Next
        End If
        Dim ret As List(Of Integer) = New List(Of Integer)
        ret.AddRange(range)
        ret.RemoveAll(Function(n) n = BLANK)
        HandRange = ret
    End Function
    Private Function TakeHand(ByVal koma As Integer) As Array
        Dim b As Button
        pop = koma
        'If koma <= 14 Then
        'pop_id = GetTegomaId(koma - 1, WHITE)
        'Else
        'pop_id = GetTegomaId(koma - 1, BLACK)
        'End If
        range = all.Clone()
        For i = 0 To 80 Step 1
            If board(i) <> 0 Then
                range(i) = BLANK
            End If
        Next
        If koma = 1 Then
            For i = 0 To 8 Step 1
                range(i) = BLANK
            Next
            For i = 0 To 8 Step 1
                For j = 0 To 8
                    If board(i + j * 9) = 1 Then
                        For k = 0 To 8
                            range(i + k * 9) = BLANK
                        Next
                    End If
                Next
            Next
        End If
        If koma = 2 Then
            For i = 0 To 8 Step 1
                range(i) = BLANK
            Next
        End If
        If koma = 3 Then
            For i = 0 To 17 Step 1
                range(i) = BLANK
            Next
        End If
        If koma = 15 Then
            For i = 72 To 80 Step 1
                range(i) = BLANK
            Next
            For i = 0 To 8 Step 1
                For j = 0 To 8
                    If board(i + j * 9) = 15 Then
                        For k = 0 To 8
                            range(i + k * 9) = BLANK
                        Next
                    End If
                Next
            Next
        End If
        If koma = 16 Then
            For i = 72 To 80 Step 1
                range(i) = BLANK
            Next
        End If
        If koma = 17 Then
            For i = 63 To 80 Step 1
                range(i) = BLANK
            Next
        End If
        If komaundo <> BLANK Then
            range = {}
            DispAll()
            komaundo = BLANK
            state = ST_FREE
            TakeHand = range.Clone
            Exit Function
        Else
            komaundo = koma
        End If
        If 1 <= koma Or koma <= 14 Then
            state = ST_WHITE_MOVE
            For c = 0 To range.Length - 1 Step 1
                b = GetButton(range(c))
                b.BackColor = Color.SandyBrown
                If REVERSE_BUTTON Then
                    SetButtonImage(b)
                End If
            Next
        End If
        If 15 <= koma Then
            state = ST_BLACK_MOVE
            For c = 0 To range.Length - 1 Step 1
                b = GetButton(range(c))
                b.BackColor = Color.DarkOrange
                If REVERSE_BUTTON Then
                    SetButtonImage(b)
                End If
            Next
        End If
        TakeHand = range.Clone
    End Function
    Private Sub DispHand()
        Dim tw = {0, 0, 0, 0, 0, 0, 0, 0}
        Dim tb = {0, 0, 0, 0, 0, 0, 0, 0}
        Dim i As Integer
        For i = 0 To tegomaw.Count - 1 Step 1
            Dim koma = Piece(tegomaw(i)).omote
            tw(koma - 1) += 1
        Next
        For i = 0 To tegomab.Count - 1 Step 1
            Dim koma = Piece(tegomab(i)).omote
            tb(koma - 1) += 1
        Next

        For i = 1 To tw.Length Step 1
            Dim btn As Button = GetHandWhite(i)
            Dim num As Integer = tw(i - 1)
            btn.Text = GetKomaName(i)
            If num >= 2 Then
                btn.Text += (vbCrLf + Str(tw(i - 1)))
            End If
            If tw(i - 1) > 0 Then
                btn.Visible = True
                SetButtonImage(btn)
            Else
                btn.Visible = False
            End If
        Next
        For i = 1 To tb.Length Step 1
            Dim num As Integer = tb(i - 1)
            Dim btn As Button = GetHandBlack(i)
            btn.Text = GetKomaName(i)
            If num >= 2 Then
                btn.Text += (vbCrLf + Str(tb(i - 1)))
            End If
            If tb(i - 1) > 0 Then
                btn.Visible = True
                SetButtonImage(btn)
            Else
                btn.Visible = False
            End If
        Next
    End Sub
    Private Sub AddYomi(ByVal locate As Integer)
        Dim x As Integer
        Dim y As Integer
        Dim koma As String
        Dim uchi As String
        Dim triangle As String
        Dim pos As String
        Dim nari As String
        Dim wb As Integer
        If IsWhite(locate) = True Then
            wb = 1
            triangle = "▲"
        Else
            wb = -1
            triangle = "▽"
        End If
        x = locate Mod 9 + 1
        y = Int(locate / 9) + 1
        If komaundo <> BLANK Then
            uchi = "打"
        Else
            uchi = ""
        End If
        If locate = kihumem Then
            pos = " 同"
        Else
            pos = Str(x) + GetKanji(y)
        End If
        If narimem <> BLANK And narimem <> board(locate) Then
            koma = GetKomaName(narimem)
            nari = "成"
        Else
            koma = GetKomaName(board(locate))
            nari = ""
        End If
        ListBox1.Items.Add("Y" + triangle + pos + koma + GetSoeji(locate) + uchi + nari)
        ListBox1.Items.Add(Hyouka() * ROBO_TEBAN * wb)
        ListBox1.TopIndex = ListBox1.Items.Count - 1
        kihumem = locate
        narimem = BLANK
    End Sub
    Dim ModosiIdx As Integer = 0
    Dim DummyIdx As Integer = 0
    Private Sub MakeMove(ByRef d As MoveData, ByVal mov As Boolean)
        DispSum("MakeMove: Start")
        If BLANK <> d.from Then
            d.src_kind = board(d.from)
        End If
        d.dst_kind = board(d._to)
        If d.hand <> BLANK Then
            Dim id = GetTegomaIDFromKind(d.hand, d.teban)
            DropHand(d.hand, d._to, id)
            DispSum("MakeMove: AfterDrop")
            GoTo LOG_WRITE
        End If
        'd.capture = KomaTori(d._to)
        If mov Then
            narimem = board(d.from)
        End If
        DispSum("MakeMove: BeforeMove")
        d.capture = ClassUp(d.from, d._to, d.komaID)
        Dim s As String = "AfterClassUp:capture = " + d.capture.ToString + ",src:" + d.from.ToString + ",dst:" + d._to.ToString + ",id:" + d.komaID.ToString + ",kind:" + d.src_kind.ToString
        DispSum(s)
LOG_WRITE:
        If DEBUG_LOG Then
            AddYomi(d._to)
        End If
        modosi.Push(d)
        DispSum("MakeMove: End")
        IdentifyMissingOrExtra()
        '評価値更新
        Dim deltaW As Buffer
        Dim deltaB As Buffer
        Dim undoInfo As New UndoInfo
        UpdateKingPos(d)
        UpdateEval(d, deltaW, deltaB)

        ' スタックに差分を保存
        undoInfo.deltaW = deltaW
        undoInfo.deltaB = deltaB
        undoStack.Push(undoInfo)
        DispEvalDifference("MakeMoveEval:")
    End Sub
    Private Sub DispEvalDifference(ByVal s As String)
        If DEBUG Then
            Dim H = Hyouka()
            Dim D = currentEval / 200
            Console.WriteLine(s & "H = " & H.ToString & ",D = " & D.ToString)
        End If
    End Sub
    Private Sub UnmakeMove()
        Dim d As MoveData = modosi.Pop
        If d.hand <> BLANK Then
            ReverseDrop(d.komaID, d.teban)
            'board(d.r2) = 0
            SetBoard(BLANK, d._to, d.komaID)
            GoTo CALC_UNDO_EVAL
        End If
        'board(d.r) = d.src
        ClassDown(d._to, d.from, d.komaID, d.src_kind)
        If d.capture <> BLANK And d.capture <> DUMMY_ID Then
            ReverseCapture(BLANK, d._to, d.capture, d.dst_kind, d.teban)
        End If
        DispSum("UnmakeMove")
CALC_UNDO_EVAL:
        Dim undoInfo As UndoInfo = undoStack.Pop()
        UnmakeEval(undoInfo.deltaW, undoInfo.deltaB)
        DispEvalDifference("UnmakeMoveEval:")
    End Sub
    Public Structure UndoInfo
        Public from As Integer
        Public _to As Integer
        Public captured As Integer
        Public deltaW As Buffer
        Public deltaB As Buffer
    End Structure
    Dim undoStack As Stack(Of UndoInfo) = New Stack(Of UndoInfo)
    Private Function Question() As Boolean
        Question = True
        If My.Computer.Keyboard.ShiftKeyDown Then
            Question = False
        End If
        If My.Computer.Keyboard.CtrlKeyDown Then
            Question = False
        End If
    End Function
    Private Function ClassUp(ByVal from As Integer, ByVal dst As Integer, ByVal id As Integer) As Integer
        Dim unit As Integer
        unit = board(from)
        Dim unit_up As Integer = unit
        If IsWhite(from) And ((0 <= from And from <= 26) Or (0 <= dst And dst <= 26)) Then
            If Question() = False Then
                GoTo SET_BOARD
            End If
            Select Case unit
                Case 1, 2, 3, 4
                    unit_up = unit + 8
                Case 6, 7
                    unit_up = unit + 7
            End Select
        End If
        If IsBlack(from) And ((54 <= from And from <= 80) Or (54 <= dst And dst <= 80)) Then
            Select Case unit
                Case 15, 16, 17, 18
                    unit_up = unit + 8
                Case 20, 21
                    unit_up = unit + 7
            End Select
        End If
SET_BOARD:
        If unit <> unit_up Then
            Piece(id).kind = unit_up
        Else
            Piece(id).kind = unit
        End If
        DispSum("ClassUp:SetBoard:Before")
        Return SetBoard(from, dst, id)
    End Function
    Private Function ClassDown(ByVal from As Integer, ByVal dst As Integer, ByVal id As Integer, ByVal kind As Integer) As Integer
        Dim unit_up As Integer
        unit_up = board(from)
        Dim unit As Integer = kind
        If unit <> unit_up Then
            Piece(id).kind = unit
        Else
            Piece(id).kind = unit_up
        End If
        DispSum("ClassDown:SetBoard:Before")
        Return SetBoard(from, dst, id)
    End Function


    Private Function ClassDown(ByVal kind As Integer) As Integer
        Dim unit As Integer = kind
        Select Case unit
            Case 9, 10, 11, 12
                unit = unit - 8
            Case 13, 14
                unit = unit - 7
            Case 23, 24, 25, 26
                unit = unit - 8
            Case 27, 28
                unit = unit - 7
        End Select
        Return unit
    End Function
    Private Function Ura_Omote(ByVal t As Integer) As Integer
        Static table As Array = {0, 1, 2, 3, 4, 5, 6, 7, 8, 1, 2, 3, 4, 6, 7, 1, 2, 3, 4, 5, 6, 7, 8, 1, 2, 3, 4, 6, 7}
        Return table(t)
    End Function
    Private Function KomaTori(ByVal locate) As Integer
        Dim t As Integer
        t = board(locate)
        If t = 0 Then
            Return DUMMY_ID
        End If
        Dim id = FindID(locate)
        If id = DUMMY_ID Then
            id = FromKind(board(locate), locate)
        End If
        Dim wb = WHITE
        If 15 <= t Then
            t = Ura_Omote(t)
            'tegomaw(t) = tegomaw(t) + 1
            If 0 <= id And id < 40 Then
                AddTegomaW(id)
                Console.WriteLine("先手が " & (id.ToString) & " を追加: ")
            End If
        ElseIf 1 <= t And t <= 14 Then
            wb = BLACK
            t = Ura_Omote(t)
            'tegomab(t) = tegomab(t) + 1
            If 0 <= id And id < 40 Then
                AddTegomaB(id)
                Console.WriteLine("後手が " & (id.ToString) & " を追加: ")
            End If
        End If
        If id <> DUMMY_ID Then
            Piece(id).captured = wb
            Piece(id).owner = wb
            Piece(id).kind = t
            Piece(id).place = BLANK
        End If
        Return id
    End Function
    Private Sub ReverseDrop(ByVal id, ByVal teban)
        If teban = WHITE Then
            If 0 <= id And id < 40 Then
                RemoveTegomaB(id)
            End If
        Else
            If 0 <= id And id < 40 Then
                RemoveTegomaW(id)
            End If
        End If
        If id <> DUMMY_ID Then
            Piece(id).captured = teban
            Piece(id).owner = teban
        End If
    End Sub
    Private Sub DropHand(ByVal hand As Integer, ByVal locate As Integer, ByVal id As Integer)
        'board(locate) = t
        SetBoardKind(BLANK, locate, id, hand)
        If id = DUMMY_ID Then
            Exit Sub
        End If
        Dim p As PieceID = Piece(id)
        Dim t As Integer = hand
        If 15 <= t Then
            't = Ura_Omote(t)
            'tegomab(t) = tegomab(t) - 1
            RemoveTegomaB(id)
        ElseIf 1 <= t And t <= 14 Then
            't = Ura_Omote(t)
            'tegomaw(t) = tegomaw(t) - 1
            RemoveTegomaW(id)
        End If
        Piece(id).kind = t
        Piece(id).place = locate
        Piece(id).captured = 0

    End Sub
    Private Sub ReverseCapture(ByVal org, ByVal locate, ByVal id, ByVal kind, ByVal teban)
        'board(locate) = t
        SetBoardKind(org, locate, id, kind)
        If teban = WHITE Then
            RemoveTegomaW(id)
        Else
            RemoveTegomaB(id)
        End If
        Exit Sub
    End Sub

    Private Function CalculateSum() As Integer
        Dim n = 0
        For i = 0 To 80
            If board(i) <> 0 Then
                n += 1
            End If
        Next
        Dim w = tegomaw.Count
        Dim b = tegomab.Count
        Dim sum = n + w + b
        Return sum
    End Function
    Private Sub DispSum(ByVal s As String)
        If DEBUG = False Then
            Exit Sub
        End If
        Dim n = 0
        For i = 0 To 80
            If board(i) <> 0 Then
                n += 1
            End If
        Next
        Dim w = tegomaw.Count
        Dim b = tegomab.Count
        Dim sum = n + w + b
        If sum <> 40 Then
            Console.WriteLine(s & "sumError! sum:" & sum.ToString & ",n:" & n.ToString & ",w:" & w.ToString & ",b:" & b.ToString)
        End If
    End Sub
    Private Sub AddKihu(ByVal locate As Integer)
        Dim x As Integer
        Dim y As Integer
        Dim koma As String
        Dim uchi As String
        Dim triangle As String
        Dim pos As String
        Dim nari As String
        If IsWhite(locate) = True Then
            triangle = "▲"
        Else
            triangle = "▽"
        End If
        locate = locate
        x = locate Mod 9 + 1
        y = Int(locate / 9) + 1
        If komaundo <> BLANK Then
            uchi = "打"
        Else
            uchi = ""
        End If
        If locate = kihumem Then
            pos = " 同"
        Else
            pos = Str(x) + GetKanji(y)
        End If
        If narimem <> BLANK And narimem <> board(locate) Then
            koma = GetKomaName(narimem)
            nari = "成"
        Else
            koma = GetKomaName(board(locate))
            nari = ""
        End If
        ListBox1.Items.Add(triangle + pos + koma + GetSoeji(locate) + uchi + nari)
        ListBox1.TopIndex = ListBox1.Items.Count - 1
        kihumem = locate
        narimem = BLANK
    End Sub
    Private Function GetKomaName(ByVal index As Integer) As String
        If 15 <= index Then
            index = index - 14
        End If
        GetKomaName = komaname(index)
    End Function
    Private Function GetKanji(ByVal num As Integer) As String
        Dim k As Array
        k = {"零", "一", "二", "三", "四", "五", "六", "七", "八", "九"}
        GetKanji = k(num)
    End Function
    Private Function GetSoeji(ByVal locate As Integer) As String
        Dim prev As Array
        Dim Range As Array
        Dim Kouho As Array
        Dim ki As Integer
        Dim x As Integer
        Dim y As Integer
        Dim ubuf As Integer
        Dim lx As Integer
        Dim ly As Integer
        Dim lbuf As Integer
        Dim dx As Integer
        Dim dy As Integer
        Dim xc As Integer
        Dim yc As Integer
        Dim dl As Integer
        Dim lr As String
        Dim ud As String
        Kouho = {BLANK, BLANK, BLANK, BLANK, BLANK, BLANK}
        ki = 0
        lr = ""
        ud = ""
        prev = board.Clone
        If undo = BLANK Then
            GetSoeji = ""
            Exit Function
        End If
        prev(undo) = board(locate)
        prev(locate) = 0
        For i = 0 To 80 Step 1
            If i = undo Then
            ElseIf prev(i) = prev(undo) Then
                Range = UnitRange(i).ToArray
                For j = 0 To Range.Length - 1 Step 1
                    If Range(j) = locate Then
                        Kouho(ki) = i
                    End If
                Next
            End If
        Next
        lbuf = locate
        lx = lbuf Mod 9
        ly = Int(lbuf / 9)
        ubuf = undo
        x = ubuf Mod 9
        y = Int(ubuf / 9)
        xc = 0
        yc = 0
        For i = 0 To Kouho.Length - 1 Step 1
            If Kouho(i) <> BLANK Then
                dl = Kouho(i)
                dx = dl Mod 9
                dy = Int(dl / 9)
                If x = dx Then
                    xc = xc + 1
                End If
                If y = dy Then
                    yc = yc + 1
                End If
            End If
        Next

        For i = 0 To Kouho.Length - 1 Step 1
            If Kouho(i) <> BLANK Then
                dl = Kouho(i)
                dx = dl - Int(dl / 9) * 9
                dy = Int(dl / 9)
                If 1 <= yc Then
                    If state = ST_WHITE_CHOOSE Then
                        If x < dx Then
                            lr = "右"
                        ElseIf dx < x Then
                            lr = "左"
                        End If
                    ElseIf state = ST_BLACK_CHOOSE Then
                        If x < dx Then
                            lr = "左"
                        ElseIf dx < x Then
                            lr = "右"
                        End If
                    End If
                End If
                If 0 = xc Then
                    If state = ST_WHITE_CHOOSE Then
                        If y < dy Then
                            ud = "引"
                        ElseIf dy < y Then
                            ud = "上"
                        End If
                    ElseIf state = ST_BLACK_CHOOSE Then
                        If y < dy Then
                            ud = "上"
                        ElseIf dy < y Then
                            ud = "引"
                        End If
                    End If
                End If
            End If
        Next
        GetSoeji = lr + ud
    End Function
    Private Sub Button1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button1.Click
        UnitClick(1)
    End Sub
    Private Sub Button2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button2.Click
        UnitClick(2)
    End Sub
    Private Sub Button3_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button3.Click
        UnitClick(3)
    End Sub
    Private Sub Button4_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button4.Click
        UnitClick(4)
    End Sub
    Private Sub Button5_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button5.Click
        UnitClick(5)
    End Sub
    Private Sub Button6_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button6.Click
        UnitClick(6)
    End Sub
    Private Sub Button7_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button7.Click
        UnitClick(7)
    End Sub
    Private Sub Button8_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button8.Click
        UnitClick(8)
    End Sub
    Private Sub Button9_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button9.Click
        UnitClick(9)
    End Sub
    Private Sub Button10_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button10.Click
        UnitClick(10)
    End Sub
    Private Sub Button11_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button11.Click
        UnitClick(11)
    End Sub
    Private Sub Button12_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button12.Click
        UnitClick(12)
    End Sub
    Private Sub Button13_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button13.Click
        UnitClick(13)
    End Sub
    Private Sub Button14_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button14.Click
        UnitClick(14)
    End Sub
    Private Sub Button15_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button15.Click
        UnitClick(15)
    End Sub
    Private Sub Button16_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button16.Click
        UnitClick(16)
    End Sub
    Private Sub Button17_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button17.Click
        UnitClick(17)
    End Sub
    Private Sub Button18_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button18.Click
        UnitClick(18)
    End Sub
    Private Sub Button19_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button19.Click
        UnitClick(19)
    End Sub
    Private Sub Button20_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button20.Click
        UnitClick(20)
    End Sub
    Private Sub Button21_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button21.Click
        UnitClick(21)
    End Sub
    Private Sub Button22_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button22.Click
        UnitClick(22)
    End Sub
    Private Sub Button23_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button23.Click
        UnitClick(23)
    End Sub
    Private Sub Button24_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button24.Click
        UnitClick(24)
    End Sub
    Private Sub Button25_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button25.Click
        UnitClick(25)
    End Sub
    Private Sub Button26_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button26.Click
        UnitClick(26)
    End Sub
    Private Sub Button27_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button27.Click
        UnitClick(27)
    End Sub
    Private Sub Button28_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button28.Click
        UnitClick(28)
    End Sub
    Private Sub Button29_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button29.Click
        UnitClick(29)
    End Sub
    Private Sub Button30_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button30.Click
        UnitClick(30)
    End Sub
    Private Sub Button31_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button31.Click
        UnitClick(31)
    End Sub
    Private Sub Button32_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button32.Click
        UnitClick(32)
    End Sub
    Private Sub Button33_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button33.Click
        UnitClick(33)
    End Sub
    Private Sub Button34_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button34.Click
        UnitClick(34)
    End Sub
    Private Sub Button35_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button35.Click
        UnitClick(35)
    End Sub
    Private Sub Button36_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button36.Click
        UnitClick(36)
    End Sub
    Private Sub Button37_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button37.Click
        UnitClick(37)
    End Sub
    Private Sub Button38_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button38.Click
        UnitClick(38)
    End Sub
    Private Sub Button39_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button39.Click
        UnitClick(39)
    End Sub
    Private Sub Button40_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button40.Click
        UnitClick(40)
    End Sub
    Private Sub Button41_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button41.Click
        UnitClick(41)
    End Sub
    Private Sub Button42_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button42.Click
        UnitClick(42)
    End Sub
    Private Sub Button43_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button43.Click
        UnitClick(43)
    End Sub
    Private Sub Button44_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button44.Click
        UnitClick(44)
    End Sub
    Private Sub Button45_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button45.Click
        UnitClick(45)
    End Sub
    Private Sub Button46_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button46.Click
        UnitClick(46)
    End Sub
    Private Sub Button47_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button47.Click
        UnitClick(47)
    End Sub
    Private Sub Button48_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button48.Click
        UnitClick(48)
    End Sub
    Private Sub Button49_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button49.Click
        UnitClick(49)
    End Sub
    Private Sub Button50_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button50.Click
        UnitClick(50)
    End Sub
    Private Sub Button51_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button51.Click
        UnitClick(51)
    End Sub
    Private Sub Button52_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button52.Click
        UnitClick(52)
    End Sub
    Private Sub Button53_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button53.Click
        UnitClick(53)
    End Sub
    Private Sub Button54_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button54.Click
        UnitClick(54)
    End Sub
    Private Sub Button55_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button55.Click
        UnitClick(55)
    End Sub
    Private Sub Button56_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button56.Click
        UnitClick(56)
    End Sub
    Private Sub Button57_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button57.Click
        UnitClick(57)
    End Sub
    Private Sub Button58_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button58.Click
        UnitClick(58)
    End Sub
    Private Sub Button59_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button59.Click
        UnitClick(59)
    End Sub
    Private Sub Button60_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button60.Click
        UnitClick(60)
    End Sub
    Private Sub Button61_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button61.Click
        UnitClick(61)
    End Sub
    Private Sub Button62_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button62.Click
        UnitClick(62)
    End Sub
    Private Sub Button63_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button63.Click
        UnitClick(63)
    End Sub
    Private Sub Button64_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button64.Click
        UnitClick(64)
    End Sub
    Private Sub Button65_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button65.Click
        UnitClick(65)
    End Sub
    Private Sub Button66_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button66.Click
        UnitClick(66)
    End Sub
    Private Sub Button67_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button67.Click
        UnitClick(67)
    End Sub
    Private Sub Button68_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button68.Click
        UnitClick(68)
    End Sub
    Private Sub Button69_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button69.Click
        UnitClick(69)
    End Sub
    Private Sub Button70_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button70.Click
        UnitClick(70)
    End Sub
    Private Sub Button71_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button71.Click
        UnitClick(71)
    End Sub
    Private Sub Button72_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button72.Click
        UnitClick(72)
    End Sub
    Private Sub Button73_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button73.Click
        UnitClick(73)
    End Sub
    Private Sub Button74_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button74.Click
        UnitClick(74)
    End Sub
    Private Sub Button75_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button75.Click
        UnitClick(75)
    End Sub
    Private Sub Button76_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button76.Click
        UnitClick(76)
    End Sub
    Private Sub Button77_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button77.Click
        UnitClick(77)
    End Sub
    Private Sub Button78_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button78.Click
        UnitClick(78)
    End Sub
    Private Sub Button79_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button79.Click
        UnitClick(79)
    End Sub
    Private Sub Button80_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button80.Click
        UnitClick(80)
    End Sub
    Private Sub Button81_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button81.Click
        UnitClick(81)
    End Sub
    Private Function GetButton(ByVal locate As Integer) As Button
        If locate < 0 Or 80 < locate Then
            'RichTextBox1.Text += "error"
        End If
        locate = locate + 1
        GetButton = Button255
        If locate = 1 Then
            GetButton = Button1
        End If
        If locate = 2 Then
            GetButton = Button2
        End If
        If locate = 3 Then
            GetButton = Button3
        End If
        If locate = 4 Then
            GetButton = Button4
        End If
        If locate = 5 Then
            GetButton = Button5
        End If
        If locate = 6 Then
            GetButton = Button6
        End If
        If locate = 7 Then
            GetButton = Button7
        End If
        If locate = 8 Then
            GetButton = Button8
        End If
        If locate = 9 Then
            GetButton = Button9
        End If
        If locate = 10 Then
            GetButton = Button10
        End If
        If locate = 11 Then
            GetButton = Button11
        End If
        If locate = 12 Then
            GetButton = Button12
        End If
        If locate = 13 Then
            GetButton = Button13
        End If
        If locate = 14 Then
            GetButton = Button14
        End If
        If locate = 15 Then
            GetButton = Button15
        End If
        If locate = 16 Then
            GetButton = Button16
        End If
        If locate = 17 Then
            GetButton = Button17
        End If
        If locate = 18 Then
            GetButton = Button18
        End If
        If locate = 19 Then
            GetButton = Button19
        End If
        If locate = 20 Then
            GetButton = Button20
        End If
        If locate = 21 Then
            GetButton = Button21
        End If
        If locate = 22 Then
            GetButton = Button22
        End If
        If locate = 23 Then
            GetButton = Button23
        End If
        If locate = 24 Then
            GetButton = Button24
        End If
        If locate = 25 Then
            GetButton = Button25
        End If
        If locate = 26 Then
            GetButton = Button26
        End If
        If locate = 27 Then
            GetButton = Button27
        End If
        If locate = 28 Then
            GetButton = Button28
        End If
        If locate = 29 Then
            GetButton = Button29
        End If
        If locate = 30 Then
            GetButton = Button30
        End If
        If locate = 31 Then
            GetButton = Button31
        End If
        If locate = 32 Then
            GetButton = Button32
        End If
        If locate = 33 Then
            GetButton = Button33
        End If
        If locate = 34 Then
            GetButton = Button34
        End If
        If locate = 35 Then
            GetButton = Button35
        End If
        If locate = 36 Then
            GetButton = Button36
        End If
        If locate = 37 Then
            GetButton = Button37
        End If
        If locate = 38 Then
            GetButton = Button38
        End If
        If locate = 39 Then
            GetButton = Button39
        End If
        If locate = 40 Then
            GetButton = Button40
        End If
        If locate = 41 Then
            GetButton = Button41
        End If
        If locate = 42 Then
            GetButton = Button42
        End If
        If locate = 43 Then
            GetButton = Button43
        End If
        If locate = 44 Then
            GetButton = Button44
        End If
        If locate = 45 Then
            GetButton = Button45
        End If
        If locate = 46 Then
            GetButton = Button46
        End If
        If locate = 47 Then
            GetButton = Button47
        End If
        If locate = 48 Then
            GetButton = Button48
        End If
        If locate = 49 Then
            GetButton = Button49
        End If
        If locate = 50 Then
            GetButton = Button50
        End If
        If locate = 51 Then
            GetButton = Button51
        End If
        If locate = 52 Then
            GetButton = Button52
        End If
        If locate = 53 Then
            GetButton = Button53
        End If
        If locate = 54 Then
            GetButton = Button54
        End If
        If locate = 55 Then
            GetButton = Button55
        End If
        If locate = 56 Then
            GetButton = Button56
        End If
        If locate = 57 Then
            GetButton = Button57
        End If
        If locate = 58 Then
            GetButton = Button58
        End If
        If locate = 59 Then
            GetButton = Button59
        End If
        If locate = 60 Then
            GetButton = Button60
        End If
        If locate = 61 Then
            GetButton = Button61
        End If
        If locate = 62 Then
            GetButton = Button62
        End If
        If locate = 63 Then
            GetButton = Button63
        End If
        If locate = 64 Then
            GetButton = Button64
        End If
        If locate = 65 Then
            GetButton = Button65
        End If
        If locate = 66 Then
            GetButton = Button66
        End If
        If locate = 67 Then
            GetButton = Button67
        End If
        If locate = 68 Then
            GetButton = Button68
        End If
        If locate = 69 Then
            GetButton = Button69
        End If
        If locate = 70 Then
            GetButton = Button70
        End If
        If locate = 71 Then
            GetButton = Button71
        End If
        If locate = 72 Then
            GetButton = Button72
        End If
        If locate = 73 Then
            GetButton = Button73
        End If
        If locate = 74 Then
            GetButton = Button74
        End If
        If locate = 75 Then
            GetButton = Button75
        End If
        If locate = 76 Then
            GetButton = Button76
        End If
        If locate = 77 Then
            GetButton = Button77
        End If
        If locate = 78 Then
            GetButton = Button78
        End If
        If locate = 79 Then
            GetButton = Button79
        End If
        If locate = 80 Then
            GetButton = Button80
        End If
        If locate = 81 Then
            GetButton = Button81
        End If
    End Function
    Private Function GetHandWhite(ByVal locate As Integer) As Button
        GetHandWhite = Button255
        If locate = 1 Then
            GetHandWhite = ButtonA1
        End If
        If locate = 2 Then
            GetHandWhite = ButtonA2
        End If
        If locate = 3 Then
            GetHandWhite = ButtonA3
        End If
        If locate = 4 Then
            GetHandWhite = ButtonA4
        End If
        If locate = 5 Then
            GetHandWhite = ButtonA5
        End If
        If locate = 6 Then
            GetHandWhite = ButtonA6
        End If
        If locate = 7 Then
            GetHandWhite = ButtonA7
        End If
        If locate = 8 Then
            GetHandWhite = ButtonA8
        End If
    End Function
    Private Function GetHandBlack(ByVal locate As Integer) As Button
        GetHandBlack = Button255
        If locate = 1 Then
            GetHandBlack = ButtonB1
        End If
        If locate = 2 Then
            GetHandBlack = ButtonB2
        End If
        If locate = 3 Then
            GetHandBlack = ButtonB3
        End If
        If locate = 4 Then
            GetHandBlack = ButtonB4
        End If
        If locate = 5 Then
            GetHandBlack = ButtonB5
        End If
        If locate = 6 Then
            GetHandBlack = ButtonB6
        End If
        If locate = 7 Then
            GetHandBlack = ButtonB7
        End If
        If locate = 8 Then
            GetHandBlack = ButtonB8
        End If
    End Function
    Private Sub ButtonA1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonA1.Click
        TakeHand(1)
    End Sub
    Private Sub ButtonA2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonA2.Click
        TakeHand(2)
    End Sub
    Private Sub ButtonA3_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonA3.Click
        TakeHand(3)
    End Sub
    Private Sub ButtonA4_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonA4.Click
        TakeHand(4)
    End Sub
    Private Sub ButtonA5_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonA5.Click
        TakeHand(5)
    End Sub
    Private Sub ButtonA6_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonA6.Click
        TakeHand(6)
    End Sub
    Private Sub ButtonA7_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonA7.Click
        TakeHand(7)
    End Sub
    Private Sub ButtonA8_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonA8.Click
        TakeHand(8)
    End Sub
    Private Sub ButtonB1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonB1.Click
        TakeHand(15)
    End Sub
    Private Sub ButtonB2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonB2.Click
        TakeHand(16)
    End Sub
    Private Sub ButtonB3_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonB3.Click
        TakeHand(17)
    End Sub
    Private Sub ButtonB4_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonB4.Click
        TakeHand(18)
    End Sub
    Private Sub ButtonB5_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonB5.Click
        TakeHand(19)
    End Sub
    Private Sub ButtonB6_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonB6.Click
        TakeHand(20)
    End Sub
    Private Sub ButtonB7_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonB7.Click
        TakeHand(21)
    End Sub
    Private Sub ButtonB8_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonB8.Click
        TakeHand(22)
    End Sub
    Private Sub ButtonRobo_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonRobo.Click
        RobotMove(ROBO_TEBAN)
    End Sub

    Private Sub Button82_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button82.Click
        UnmakeMove()
        DispAll()
    End Sub

    Private Sub RichTextBox1_TextChanged(sender As Object, e As EventArgs) Handles RichTextBox1.TextChanged

    End Sub
    Private Sub DispBitBoard()
        Dim s1 As String = ""
        Dim s2 As String = ""
        Dim s3 As String = ""
        Dim s4 As String = ""
        Dim mask As Int64 = &B111111111
        For i = 0 To 6 Step 1
            Dim m As Int64 = mask
            Dim d As Int64 = (bb_black.b1 >> (i * 9)) And m
            For j = 0 To 8 Step 1
                Dim b As Int64 = (d >> j) And 1
                s1 += Convert.ToString(b, 2)
            Next
            s1 += vbCrLf
        Next
        For i = 0 To 1 Step 1
            Dim m As Int64 = mask
            Dim d As Int64 = (bb_black.b2 >> (i * 9)) And m
            For j = 0 To 8 Step 1
                Dim b As Int64 = (d >> j) And 1
                s2 += Convert.ToString(b, 2)
            Next
            s2 += vbCrLf
        Next
        For i = 0 To 6 Step 1
            Dim m As Int64 = mask
            Dim d As Int64 = (bb_white.b1 >> (i * 9)) And m
            For j = 0 To 8 Step 1
                Dim b As Int64 = (d >> j) And 1
                s3 += Convert.ToString(b, 2)
            Next
            s3 += vbCrLf
        Next
        For i = 0 To 1 Step 1
            Dim m As Int64 = mask
            Dim d As Int64 = (bb_white.b2 >> (i * 9)) And m
            For j = 0 To 8 Step 1
                Dim b As Int64 = (d >> j) And 1
                s4 += Convert.ToString(b, 2)
            Next
            s4 += vbCrLf
        Next
        Dim s5 As String = "" + vbCrLf + vbCrLf
        Dim n = 0
        For i = 0 To 8
            For j = 0 To 8
                s5 += board(n).ToString + ","
                n += 1
            Next
            s5 += vbCrLf
        Next

        RichTextBox1.Clear()
        RichTextBox1.Text = s1 + s2 + vbCrLf + s3 + s4 + s5
    End Sub
    Private Sub DispEval()
        Dim s1 As String = "先手駒得:" + Convert.ToString(WTop.komatoku)
        Dim s2 As String = "先手駒位:" + Convert.ToString(WTop.komaichi)
        Dim s3 As String = "先手駒利:" + Convert.ToString(WTop.komakiki)
        Dim s4 As String = "後手駒得:" + Convert.ToString(BTop.komatoku)
        Dim s5 As String = "後手駒位:" + Convert.ToString(BTop.komaichi)
        Dim s6 As String = "後手駒利:" + Convert.ToString(BTop.komakiki)
        RichTextBox1.Clear()
        RichTextBox1.Text = s1 + vbCrLf + s2 + vbCrLf + s3 + vbCrLf + s4 + vbCrLf + s5 + vbCrLf + s6
    End Sub
    Private Sub DispPiece()
        Dim s1 As String
        Dim s2 As String
        For i = 0 To Piece.Count - 1
            Dim p = Piece(i)
            s1 += p.id.ToString.PadLeft(2) + ":" + p.omote.ToString.PadLeft(2) + "," + p.kind.ToString.PadLeft(2) + "," + p.place.ToString.PadLeft(2) + "," + p.owner.ToString.PadLeft(2) + "," + p.captured.ToString.PadLeft(2) + " "
            If i Mod 2 = 1 Then
                s1 += vbCrLf
            End If
        Next
        s1 += "W"
        For i = 0 To tegomaw.Count - 1
            s2 += tegomaw(i).ToString + ","
        Next
        s2 += vbCrLf + "B"
        For i = 0 To tegomab.Count - 1
            s2 += tegomab(i).ToString + ","
        Next
        RichTextBox1.Clear()
        RichTextBox1.Text = s1 + s2
    End Sub
    Private Sub Button83_Click(sender As Object, e As EventArgs) Handles Button83.Click
        DispBitBoard()
    End Sub

    Private Sub Button84_Click(sender As Object, e As EventArgs) Handles Button84.Click
        DispPiece()
        'DispEval()
    End Sub

    Private Sub SaveJyoseki()
        Dim encoding = System.Text.Encoding.UTF8
        Dim filePath As String = "..\..\JD\book.txt"
        Dim id As Integer = 1
        Using strm As IO.StreamWriter = New IO.StreamWriter(filePath, False, encoding)
            For Each a In _JyosekiDictionary
                Dim Text As String = id.ToString + "$" + a.Key + "$" + a.Value
                strm.WriteLine(Text)
                id = id + 1
            Next
        End Using
    End Sub
    Private Sub LoadJyoseki()
        If USE_JYOSEKI <> True Then
            Exit Sub
        End If
        Dim encoding = System.Text.Encoding.UTF8
        Dim filePath As String = "..\..\JD\book.txt"
        Dim line As String = String.Empty
        Dim arr As Array

        ' StreamReaderのインスタンスを生成する
        Using strm As IO.StreamReader = New IO.StreamReader(filePath, encoding)
            line = strm.ReadLine()
            While line IsNot Nothing
                ' 読み込んだ1行を配列に格納する
                arr = line.Split("$")
                If arr.Length = 3 Then
                    _JyosekiDictionary.Add(arr(1), arr(2))
                End If
                ' ファイルを1行読み込む
                line = strm.ReadLine()
            End While
        End Using
    End Sub
    Private Sub Button85_Click(sender As Object, e As EventArgs) Handles Button85.Click
        'save
        SaveJyoseki()
    End Sub

    Private Sub Button86_Click(sender As Object, e As EventArgs) Handles Button86.Click
        'load
        LoadJyoseki()
    End Sub
    Private Sub クリップボードにコピーToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles クリップボードにコピーToolStripMenuItem.Click
        Dim max As Integer = ListBox1.Items.Count
        Dim Clip As String = ""
        For i = 0 To max - 1
            Clip += ListBox1.Items.Item(i) + vbCrLf
        Next
        Clipboard.SetText(Clip)

    End Sub

    Private Sub DispPieceID()
        Dim s As String
        For i = 0 To 80
            Dim flag = False
            For id = 0 To 39
                If i = Piece(id).place Then
                    s += Piece(id).place.ToString.PadLeft(2)
                    flag = True
                End If
            Next
            If flag = False Then
                s += "DU"
            End If
            If i Mod 9 = 8 Then
                s += vbCrLf
            End If
        Next
        RichTextBox1.Text = s
    End Sub
    Private Sub Button87_Click(sender As Object, e As EventArgs) Handles Button87.Click
        Dim thread As New Threading.Thread(AddressOf CommunicateWithEngine)
        thread.IsBackground = True ' フォーム終了時にスレッドも終了するよう設定
        thread.Start()
    End Sub
    Private Sub AddTegomaW(ByVal id)
        If tegomaw.Find(Function(n) n = id) Then
            Console.WriteLine("DuplicateID AddTegomaW !")
        Else
            tegomaw.Add(id)
        End If
        Dim s As String = "AddTegomaW:id=" + id.ToString
        DispSum(s)
    End Sub
    Private Sub AddTegomaB(ByVal id)
        If tegomab.Find(Function(n) n = id) Then
            Console.WriteLine("DuplicateID AddTegomaB !")
        Else
            tegomab.Add(id)
        End If
        Dim s As String = "AddTegomaB:id=" + id.ToString
        DispSum(s)
    End Sub
    Private Sub RemoveTegomaW(ByVal id)
        tegomaw.Remove(id)
        Dim s As String = "RemoveTegomaW:id=" + id.ToString
        DispSum(s)
    End Sub
    Private Sub RemoveTegomaB(ByVal id)
        tegomab.Remove(id)
        Dim s As String = "RemoveTegomaB:id=" + id.ToString
        DispSum(s)
    End Sub

    Dim filePath As String = "..\..\Engine\ShogiBasicEngine.exe"
    Dim engineProcess As Process = Nothing
    Dim engineState As Integer = INIT_ENGINE
    Const INIT_ENGINE = 0
    Const START_ENGINE = 1
    Const SEND_USI = 2
    Const WAIT_MESSAGE = 3
    Private Sub CommunicateWithEngine()
        If engineState = INIT_ENGINE Then
            engineProcess = New Process()
            engineProcess.StartInfo.FileName = filePath
            engineProcess.StartInfo.RedirectStandardInput = True
            engineProcess.StartInfo.RedirectStandardOutput = True
            engineProcess.StartInfo.RedirectStandardError = True
            engineProcess.StartInfo.UseShellExecute = False
            engineProcess.StartInfo.CreateNoWindow = True ' デバッグ用に黒窓表示
            engineProcess.Start()
            engineState = START_ENGINE
            ' デバッグ用ログ
            UpdateTextBox("プロセス起動")


            ' 起動直後に終了するか、エラー出力を確認
            ' スレッドを待機状態に（プロセスを維持）
            Threading.Thread.Sleep(100) ' 無限ループでCPU負荷を抑える
            engineProcess.StandardInput.WriteLine("usi")
            engineProcess.StandardInput.Flush()
            UpdateTextBox("usi送信")

            ' 複数行読み取り
            Dim response As String
            Do
                response = engineProcess.StandardOutput.ReadLine()
                If response IsNot Nothing Then
                    UpdateTextBox("出力: " & response)
                End If
            Loop Until response Is Nothing OrElse response = "usiok" OrElse engineProcess.HasExited
        ElseIf engineState = START_ENGINE Then
            Threading.Thread.Sleep(100) ' 無限ループでCPU負荷を抑える
            engineProcess.StandardInput.WriteLine("isready")
            engineProcess.StandardInput.Flush()
            UpdateTextBox("isready送信")

            ' 複数行読み取り
            Dim response As String
            Do
                response = engineProcess.StandardOutput.ReadLine()
                If response IsNot Nothing Then
                    UpdateTextBox("出力: " & response)
                End If
            Loop Until response Is Nothing OrElse response = "usiok" OrElse engineProcess.HasExited
        Else
            UpdateTextBox("応答を読み取る")

            If engineProcess IsNot Nothing AndAlso Not engineProcess.HasExited Then
                UpdateTextBox("プロセスが終了してます")
            End If
            ' 応答を読み取る
            Dim output As String = engineProcess.StandardOutput.ReadLine()
            Dim _error As String = engineProcess.StandardError.ReadLine()

            UpdateTextBox("UIスレッドに結果を反映")
            UpdateTextBox("出力:" & output & vbCrLf & "エラー: " & _error)
            ' UIスレッドに結果を反映
            If RichTextBox1.InvokeRequired Then
                UpdateTextBox("UIスレッドに結果を反映")
                RichTextBox1.Invoke(Sub() RichTextBox1.Text = "出力: " & output & vbCrLf & "エラー: " & _error)
            Else
                RichTextBox1.Text = "出力: " & output & vbCrLf & "エラー: " & _error
            End If
            ' プロセスが終了したか確認
            If engineProcess.HasExited Then
                RichTextBox1.Invoke(Sub() RichTextBox1.Text &= vbCrLf & "プロセス終了 (コード: " & engineProcess.ExitCode & ")")
            End If
        End If
    End Sub
    Private Sub UpdateTextBox(message As String)
        If RichTextBox1.InvokeRequired Then
            RichTextBox1.Invoke(Sub() RichTextBox1.Text &= message & vbCrLf)
        Else
            RichTextBox1.Text &= message & vbCrLf
        End If
    End Sub
    Private Sub ReadCallback(ar As IAsyncResult)
        Dim reader As IO.StreamReader = DirectCast(ar.AsyncState, IO.StreamReader)
        Dim line As String = reader.ReadLine()
        If line IsNot Nothing Then
            UpdateTextBox("出力: " & line)
        End If
    End Sub
    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        DispAll()
        DrawShogiBoard(Panel1)
    End Sub
    Private Sub SetButtonImage(btn As Button)
        Dim text As String = btn.Text
        Dim isHand As Boolean = False
        Dim isBlack As Boolean = False
        If btn.BackColor = Color.Cornsilk Then
            isBlack = True
        ElseIf btn.BackColor = Color.SandyBrown And text <> "" Then
            isBlack = True
        ElseIf btn.BackColor = Color.DarkOrange And text = "" Then
            isBlack = True
        End If
        If text.Length >= 2 Then
            isHand = True
        End If
        Dim bmp As New Bitmap(btn.Width, btn.Height)
        Using g As Graphics = Graphics.FromImage(bmp)
            g.Clear(btn.BackColor)
            Dim font As New Font("MS Gothic", 14)
            If isHand Then
                font = New Font("MS Gothic", 10)
            End If
            Dim textSize As SizeF = g.MeasureString(text, font)
            Dim x As Single = (btn.Width - textSize.Width) / 2
            Dim y As Single = (btn.Height - textSize.Height) / 2
            If isBlack Then
                g.RotateTransform(180)
                g.TranslateTransform(-btn.Width, -btn.Height)
            End If
            g.DrawString(text, font, New SolidBrush(btn.ForeColor), x, y)
            ' 五角形の頂点を計算（ボタンのサイズに基づく）
            Dim points(6) As Point
            Dim centerX As Integer = btn.Width / 2
            Dim centerY As Integer = btn.Height / 2 + 10
            Dim radius As Integer = Math.Min(btn.Width, btn.Height) \ 2 ' 少し内側に

            If isBlack Then
                points(0) = New Point(1, 1)
                points(1) = New Point(btn.Width, 0)
                points(2) = New Point(btn.Width - 4, btn.Height - 7)
                points(3) = New Point(btn.Width / 2, btn.Height - 2)
                points(4) = New Point(3, btn.Height - 7)
                points(5) = New Point(1, 1)
            Else
                points(0) = New Point(centerX, 1)
                points(1) = New Point(btn.Width - 3, 5)
                points(2) = New Point(btn.Width - 1, btn.Height - 1)
                points(3) = New Point(0, btn.Height - 1)
                points(4) = New Point(3, 5)
                points(5) = New Point(centerX, 1)
            End If

            ' GraphicsPathで五角形を作成
            Dim path As New GraphicsPath()
            path.AddPolygon(points)

            ' ボタンの形状を五角形に設定
            btn.Region = New Region(path)
            btn.FlatAppearance.BorderSize = 0 ' 枠線の太さを0に設定して消す
        End Using
        btn.Text = ""
        btn.Image = bmp
    End Sub
End Class
' 2015 - 2025 Written By Kyosuke Miyazawa ShogiBasic