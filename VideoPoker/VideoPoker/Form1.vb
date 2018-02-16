Public Class Form1
    Dim img1 As Image = My.Resources.trump
    Dim T As Integer = 0
    Dim Deck(53) As Integer ' 55b, 48b, 41j, 34j
    Dim Hand(5) As Integer
    Dim Change(5) As Integer
    Dim RankNum As Integer
    Private Sub DeckSuhhle()
        Dim j = 0
        For i = 0 To 55
            If i <> 55 And i <> 48 And i <> 41 Then
                Deck(j) = i
                j = j + 1
            End If
        Next
        Dim Buff As Integer
        For i = 0 To 500
            Dim a As Integer = Rnd() * 52
            Dim b As Integer = Rnd() * 52
            Buff = Deck(a)
            Deck(a) = Deck(b)
            Deck(b) = Buff
        Next
    End Sub
    Private Function RankString(ByVal i As Integer) As String
        RankString = ""
        If i = 10 Then
            RankString = "RoyalFlush"
        End If
        If i = 9 Then
            RankString = "StraightFlush"
        End If
        If i = 8 Then
            RankString = "FourCards"
        End If
        If i = 7 Then
            RankString = "FullHouse"
        End If
        If i = 6 Then
            RankString = "Flush"
        End If
        If i = 5 Then
            RankString = "Straight"
        End If
        If i = 4 Then
            RankString = "ThreeCards"
        End If
        If i = 3 Then
            RankString = "TwoPair"
        End If
        If i = 2 Then
            RankString = "OnePair"
        End If
        If i = 1 Then
            RankString = "NoPair"
        End If
    End Function
    Private Function Rank() As String
        RankNum = 0
        Rank = ""
        'ジョーカーの判定
        For i = 0 To 4 Step 1
            Dim idx = Hand(i)
            If idx = 34 Then
                Dim MaxRank = 0
                For n = 0 To 54
                    If n <> 34 Then
                        Hand(i) = n
                        Rank()
                        If MaxRank < RankNum Then
                            MaxRank = RankNum
                        End If
                    End If
                Next
                Hand(i) = 34
                Rank = RankString(MaxRank)
                Return Rank
            End If
        Next
        Dim Num(5) As Integer
        Dim Suit As String = ""
        For i = 0 To 4 Step 1
            Dim idx = Hand(i)
            If idx = 55 Then
                Rank = ""
                Return Rank
            End If
            Dim x As Integer = 0
            Dim y As Integer = 0
            If idx >= 28 Then
                y = 7
                idx = idx - 28
            End If
            x = Int(idx / 7) 'h,s,d,c
            y = y + idx Mod 7
            Num(i) = y
            Suit = Suit + x.ToString
        Next
        'ペアチェック
        Dim NumBuff() As Integer = Num.Clone
        Dim Pair As Integer = 0
        Dim Two As Integer = 0
        Dim PairFlag As Boolean = False
        For i = 0 To 4
            For j = (i + 1) To 4
                If Num(i) = NumBuff(j) Then
                    If PairFlag = False Then
                        Pair = Pair + 1
                        NumBuff(j) = -1
                    Else
                        Two = Two + 1
                        NumBuff(j) = -1
                    End If
                End If
            Next
            If Pair > 0 Then
                PairFlag = True
            End If
        Next
        If Pair = 3 Then
            RankNum = 8
            Rank = "FourCards"
        ElseIf (Pair = 2 And Two = 1) Or (Pair = 1 And Two = 2) Then
            RankNum = 7
            Rank = "FullHouse"
        ElseIf Pair = 2 Then
            RankNum = 4
            Rank = "ThreeCards"
        ElseIf Pair = 1 And Two = 1 Then
            RankNum = 3
            Rank = "TwoPair"
        ElseIf Pair = 1 Then
            RankNum = 2
            Rank = "OnePair"
        End If
        'フラッシュチェック
        Dim Fl As Boolean = True
        For i = 1 To 4
            If Suit(0) <> Suit(i) Then
                Fl = False
            End If
        Next
        'ストレートチェック
        Dim max As Integer = 0
        Dim min As Integer = 99
        Dim Ace As Integer = 0
        For i = 0 To 4
            If max < Val(Num(i)) Then
                max = Val(Num(i))
            End If
            If 0 = Val(Num(i)) Then
                Ace = Ace + 1
            ElseIf min > Val(Num(i)) Then
                min = Val(Num(i))
            End If
        Next
        Dim St = 0
        For i = 0 To 4
            For j = 0 To 4
                If (max - i) = Val(Num(j)) Then
                    St = St + 1
                    Exit For
                End If
            Next
        Next
        Dim Royal As Boolean = False
        If max = 12 And min = 9 And St = 4 And Ace = 1 Then
            Royal = True
        End If
        If Royal And Fl Then
            RankNum = 10
            Rank = "RoyalFlush"
        ElseIf St = 5 And Fl Then
            RankNum = 9
            Rank = "StraightFlush"
        ElseIf Fl Then
            RankNum = 6
            Rank = "Flush"
        ElseIf St = 5 Or Royal Then
            RankNum = 5
            Rank = "Straight"
        End If
        If Rank = "" Then
            RankNum = 1
            Rank = "NoPair"
        End If
    End Function
    Private Function CardGraph(ByVal idx As Integer) As Rectangle
        Dim x = 0
        Dim y = 0
        If idx >= 28 Then
            x = 240
            idx = idx - 28
        End If
        x = x + (Int(idx / 7) * 60)
        y = idx Mod 7
        CardGraph = New Rectangle(x, y * 90, 61, 91)
    End Function
    Private Function Box(ByVal idx As Integer) As PictureBox
        Dim ret() As PictureBox = {PictureBox1, PictureBox2, PictureBox3, PictureBox4, PictureBox5}
        Box = ret(idx)
    End Function
    Private Sub Form1_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
        DeckSuhhle()
        For i = 0 To 4
            Hand(i) = 55
            Change(i) = 55
        Next
        Disp()
    End Sub

    Private Sub Disp()
        For i = 0 To 4 Step 1
            Dim canvas As New Bitmap(PictureBox1.Width, PictureBox1.Height)
            'ImageオブジェクトのGraphicsオブジェクトを作成する
            Dim g As Graphics = Graphics.FromImage(canvas)
            Dim srcRect As Rectangle = CardGraph(Hand(i))
            Dim desRect As New Rectangle(0, 0, srcRect.Width, srcRect.Height)
            '画像の一部を描画する
            g.DrawImage(img1, desRect, srcRect, GraphicsUnit.Pixel)
            Box(i).Image = canvas
        Next
        Label1.Text = Rank()
    End Sub
    Private Sub Button1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button1.Click
        For i = 0 To 4 Step 1
            If Hand(i) = 55 Then
                Hand(i) = Deck(T)
                T = T + 1
                If T >= 53 Then
                    DeckSuhhle()
                    T = 0
                End If
            End If
        Next
        Disp()
    End Sub

    Private Sub Hold(ByVal idx As Integer)
        If Hand(idx) <> 55 Then
            Change(idx) = Hand(idx)
            Hand(idx) = 55
        Else
            Hand(idx) = Change(idx)
        End If
    End Sub
    Private Sub PictureBox1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PictureBox1.Click
        Hold(0)
        Disp()
    End Sub
    Private Sub PictureBox2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PictureBox2.Click
        Hold(1)
        Disp()
    End Sub
    Private Sub PictureBox3_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PictureBox3.Click
        Hold(2)
        Disp()
    End Sub
    Private Sub PictureBox4_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PictureBox4.Click
        Hold(3)
        Disp()
    End Sub
    Private Sub PictureBox5_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PictureBox5.Click
        Hold(4)
        Disp()
    End Sub

    Private Sub Form1_KeyDown(ByVal sender As System.Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles MyBase.KeyDown
        If e.KeyCode = Keys.D1 Then
            Hold(0)
            Disp()
        End If
        If e.KeyCode = Keys.D2 Then
            Hold(1)
            Disp()
        End If
        If e.KeyCode = Keys.D3 Then
            Hold(2)
            Disp()
        End If
        If e.KeyCode = Keys.D4 Then
            Hold(3)
            Disp()
        End If
        If e.KeyCode = Keys.D5 Then
            Hold(4)
            Disp()
        End If
        If e.KeyCode = Keys.Enter Then
            Button1.PerformClick()
        End If
        If e.KeyCode = Keys.A Then
            For i = 0 To 4
                If Hand(i) <> 55 Then
                    Hold(i)
                End If
            Next
            Disp()
        End If
    End Sub
End Class
