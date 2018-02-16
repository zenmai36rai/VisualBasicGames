Public Class Mahjong
    Const DEBUG_MODE = False
    Const ENEMY_VISIBLE = False ' 敵の手を見せる
    Class PonState
        Public Hai
        Public Houi
        Public Sub New()
            Hai = 255
            Houi = 0
        End Sub
    End Class
    Class CheeState
        Public Hai(3)
        Public Houi
        Public Sub New()
            For i = 0 To 3
                Hai(i) = 255
            Next
            Houi = 0
        End Sub
    End Class
    Class KanState
        Public Hai
        Public Houi
        Public Sub New()
            Hai = 255
            Houi = 0
        End Sub
    End Class
    Class Player
        Public Tokuten As Integer
        Public FaceImage As Integer
        Public Kaze As Integer
        Public Tehai(13) As Integer
        Public SuteIdx As Integer
        Public PonHai(4) As PonState
        Public CheeHai(4) As CheeState
        Public KanHai(4) As KanState
        Public Sub New()
            For i = 0 To 3
                PonHai(i) = New PonState
                CheeHai(i) = New CheeState
                KanHai(i) = New KanState
            Next
        End Sub
        Public Function GetTumoIdx() As Integer
            Dim ret = 13
            For i = 0 To 3
                If PonHai(i).Hai <> 255 Then
                    ret = ret - 3
                End If
                If CheeHai(i).Hai(0) <> 255 Then
                    ret = ret - 3
                End If
                If KanHai(i).Hai <> 255 Then
                    ret = ret - 3
                End If
            Next
            Return ret
        End Function
        Public Function GetKanCount() As Integer
            Dim ret = 0
            For i = 0 To 3
                If KanHai(i).Hai <> 255 Then
                    ret = ret + 1
                End If
            Next
            Return ret
        End Function
        Public Sub Clear()
            For i = 0 To 3
                PonHai(i).Hai = 255
                KanHai(i).Hai = 255
                For j = 0 To 2
                    CheeHai(i).Hai(j) = 255
                Next
            Next
        End Sub
    End Class
    Dim Players(4) As Player
    Dim img1 As Image = My.Resources.mahjong01
    Dim img2 As Image = My.Resources.sf2
    Dim YamaHai(136) As Integer
    Dim Sutehai(100) As Integer
    Dim PicRinshan(7) As System.Windows.Forms.PictureBox
    Dim PicEnemy(42) As System.Windows.Forms.PictureBox
    Dim PicSute(100) As System.Windows.Forms.PictureBox
    Dim PicKan(16) As System.Windows.Forms.PictureBox
    Dim TumoCount As Integer
    Dim flgFirst As Boolean = True
    Dim Turn As Integer
    Dim StartFlag As Boolean
    Dim NakiHai As Integer
    Dim ButtonFlag As Integer '0:キャンセル 1:ポン 2:チー 3:カン 4:ロン
    Dim ClickLock As Boolean
    Dim CheeSelect(6) As Integer 'チーできる牌の組み合わせ。0,1:下 2,3:中 4,5:上
    Dim KanSelect(3) As Integer 'カンできる牌の種類
    Dim CheeSelectMode As Boolean
    Dim Oya As Integer
    Dim BaTurn As Integer
    Dim BaKaze As Integer
    Dim Yaku As String
    Dim AgariTen As Integer
    Dim AgariPlayer As Integer
    Private Sub Form1_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
        Top = 0
        BaTurn = 0
        Oya = Int(Rnd() * 3)
        '場風
        BaKaze = 27
        '自風
        Dim Ton = Oya
        If Ton = 0 Then
            Players(0).Kaze = 27
            Players(1).Kaze = 28
            Players(2).Kaze = 29
            Players(3).Kaze = 30
        End If
        If Ton = 1 Then
            Players(0).Kaze = 30
            Players(1).Kaze = 27
            Players(2).Kaze = 28
            Players(3).Kaze = 29
        End If
        If Ton = 2 Then
            Players(0).Kaze = 29
            Players(1).Kaze = 30
            Players(2).Kaze = 27
            Players(3).Kaze = 28
        End If
        If Ton = 3 Then
            Players(0).Kaze = 28
            Players(1).Kaze = 29
            Players(2).Kaze = 30
            Players(3).Kaze = 27
        End If
        FirstLoop()
    End Sub
    Private Sub FirstLoop()
        StartGame()
        '初巡
        If Turn = 0 Then
            PlayerTumo()
        Else
            EnemyTurn()
        End If
    End Sub
    Private Sub PlayerTumo()
        Tumo(Players(0))
        If AgariHantei(Players(0)) Then
            RonPlayer = 0
            ButtonTumo.Text = "ツモ"
            ButtonTumo.Visible = True
        End If
        If AnKanHantei() Then
            ButtonKan.Visible = True
        End If
        ButtonSort()
    End Sub
    Private Function EnemyTurn() As Boolean
        Show()
        ClickLock = True
        For i = Turn To 3
            Tumo(Players(Turn))
            If AgariHantei(Players(Turn)) Then
                MsgBox("ツモ" + +vbCrLf + Yaku)
                Return True
            End If
            Dim idx = Think(i) ' 思考ルーチン
            SuteHaiSub(idx, Sutehai, Players(Turn))
            If NakiHantei() Then
                Enabled = True
                Return False
            Else
                TurnNext()
            End If
        Next
        PlayerTumo()
        ClickLock = False
        Return False
    End Function
    Private Sub StartGame()
        StartFlag = True
        ClickLock = False
        CheeSelectMode = False
        Shuffle()
        AgariPlayer = 0
        Turn = Oya
        TumoCount = 14
        For i = 0 To 3
            For j = 0 To 12
                Players(i).Tehai(j) = YamaHai(TumoCount)
                TumoCount = TumoCount + 1
            Next
            Players(i).Tehai(13) = 255
            Sort(Players(i).Tehai)
            Players(i).SuteIdx = 0
            Players(i).Clear()
        Next
        For i = 0 To 99
            Sutehai(i) = 255
        Next
        For i = 0 To 6
            CheeSelect(i) = 255
        Next
    End Sub
    Private Sub Shuffle()
        TumoCount = 0
        For i = 0 To 135
            YamaHai(i) = Int(i / 4)
        Next
        For i = 0 To 1000
            Dim a = Int(Rnd() * 135)
            Dim b = Int(Rnd() * 135)
            Dim buff = YamaHai(a)
            YamaHai(a) = YamaHai(b)
            YamaHai(b) = buff
        Next
    End Sub
    Private Sub Sort(ByRef hai)
        Dim buff As Array = hai.Clone
        buff(13) = 255
        System.Array.Sort(buff)
        For i = 0 To 12
            hai(i) = buff(i)
        Next
    End Sub
    Private Function Box(ByVal idx As Integer) As PictureBox
        Dim ret() = {PictureBox1, PictureBox2, PictureBox3, PictureBox4, PictureBox5, PictureBox6, PictureBox7, PictureBox8, PictureBox9, PictureBox10, PictureBox11, PictureBox12, PictureBox13, PictureBox14}
        Return ret(idx)
    End Function
    Private Function Face(ByVal idx As Integer) As PictureBox
        Dim ret() = {PictureBoxFace1, PictureBoxFace2, PictureBoxFace3, PictureBoxFace4}
        Return ret(idx)
    End Function
    Private Sub DrawHai(ByRef canvas As Bitmap, ByVal x As Integer, ByVal p As PictureBox)
        'ImageオブジェクトのGraphicsオブジェクトを作成する
        Dim g As Graphics = Graphics.FromImage(canvas)
        Dim srcRect As New Rectangle((x Mod 9) * 48, Int(x / 9) * 72, 48, 72)
        Dim desRect As New Rectangle(0, 0, p.Width, p.Height)
        g.DrawImage(img1, desRect, srcRect, GraphicsUnit.Pixel)
    End Sub
    Private Sub DrawFace(ByRef canvas As Bitmap, ByVal x As Integer, ByVal p As PictureBox)
        'ImageオブジェクトのGraphicsオブジェクトを作成する
        Dim g As Graphics = Graphics.FromImage(canvas)
        Dim srcRect As New Rectangle((x Mod 4) * 96, Int(x / 4) * 96, 96, 96)
        Dim desRect As New Rectangle(0, 0, p.Width, p.Height)
        g.DrawImage(img2, desRect, srcRect, GraphicsUnit.Pixel)
    End Sub
    Private Sub Draw()
        SuspendLayout()
        If flgFirst Then
            Randomize()
            For i = 0 To 3
                Players(i) = New Player
                Players(i).Tokuten = 25000
            Next
            Players(0).FaceImage = 0
            Dim used(7) As Boolean
            For i = 0 To 6
                used(i) = False
            Next
            Dim count = 0
            While count < 3
                Dim f = Int(Rnd() * 6)
                If used(f) = False Then
                    used(f) = True
                    count = count + 1
                    Players(count).FaceImage = f + 1
                End If
            End While
            For i = 0 To 41
                PicEnemy(i) = New System.Windows.Forms.PictureBox()
                Me.Controls.Add(PicEnemy(i))
            Next
            For i = 0 To 15
                PicKan(i) = New System.Windows.Forms.PictureBox()
                Me.Controls.Add(PicKan(i))
            Next
            For i = 0 To 6
                PicRinshan(i) = New System.Windows.Forms.PictureBox()
                Me.Controls.Add(PicRinshan(i))
            Next
        End If
        For i = 0 To Players(0).GetTumoIdx()
            Box(i).Enabled = True
        Next
        For i = Players(0).GetTumoIdx() + 1 To 13
            Box(i).Enabled = False
        Next
        'カンのクリア
        For i = 0 To 15
            PicKan(i).Image = Nothing
        Next
        '顔の描画
        For i = 0 To 3
            Dim canvas As New Bitmap(PictureBoxFace1.Width, PictureBoxFace1.Height)
            'ImageオブジェクトのGraphicsオブジェクトを作成する
            DrawFace(canvas, Players(i).FaceImage, Face(i))
            Face(i).Image = canvas
        Next
        '文字の描画
        Label1.Text = GetKazeName(Players(0).Kaze) + ":" + Players(0).Tokuten.ToString
        Label2.Text = GetKazeName(Players(1).Kaze) + ":" + Players(1).Tokuten.ToString
        Label3.Text = GetKazeName(Players(2).Kaze) + ":" + Players(2).Tokuten.ToString
        Label4.Text = GetKazeName(Players(3).Kaze) + ":" + Players(3).Tokuten.ToString
        Label5.Text = GetKazeName(BaKaze)
        '王牌の描画
        Dim w = (Me.Width - 12 * 4) / 14 * 0.66
        For i = 0 To 6
            Dim x = 34
            'ドラの表示
            If 2 <= i And i <= Players(0).GetKanCount + 2 Then
                x = YamaHai(4 + (i - 2) * 2)
            End If
            PicRinshan(i).Width = w
            PicRinshan(i).Height = w * 1.5
            PicRinshan(i).Top = Me.ClientSize.Height / 2 - w * 1.5
            PicRinshan(i).Left = i * w + Me.ClientSize.Width / 2 - w * 3.5
            Dim canvas As New Bitmap(PictureBox1.Width, PictureBox1.Height)
            'ImageオブジェクトのGraphicsオブジェクトを作成する
            DrawHai(canvas, x, PicRinshan(i))
            PicRinshan(i).Image = canvas
        Next
        For i = 0 To Players(0).GetTumoIdx()
            Dim x = Players(0).Tehai(i)
            Box(i).Width = w
            Box(i).Height = w * 1.5
            Box(i).Top = Me.ClientSize.Height - w * 1.5
            Box(i).Left = i * w + 12 + Me.Width * 0.1
            If i = Players(0).GetTumoIdx() Then
                Box(i).Left = Box(i).Left + 12
            End If
            Dim canvas As New Bitmap(PictureBox1.Width, PictureBox1.Height)
            'ImageオブジェクトのGraphicsオブジェクトを作成する
            DrawHai(canvas, x, Box(i))
            Box(i).Image = canvas
        Next
        For i = 0 To 23
            Dim x = Sutehai(i)
            If flgFirst Then
                PicSute(i) = New System.Windows.Forms.PictureBox()
                Me.Controls.Add(PicSute(i))
            End If
            PicSute(i).Width = w
            PicSute(i).Height = w * 1.5
            If i < 18 Then
                PicSute(i).Left = (Me.Width / 2 - 3 * w) + (i Mod 6) * w - 12
                PicSute(i).Top = Me.Width / 2 + 3 * w + Int(i / 6) * w * 1.5 + 12
            Else
                PicSute(i).Left = (Me.Width / 2 - 3 * w) + (i Mod 6) * w - 12 + w * 6
                PicSute(i).Top = Me.Width / 2 + 3 * w + Int(i / 6) * w * 1.5 - w * 1.5 + 12
            End If
            Dim canvas As New Bitmap(PictureBox1.Width, PictureBox1.Height)
            'ImageオブジェクトのGraphicsオブジェクトを作成する
            DrawHai(canvas, x, PicSute(i))
            PicSute(i).Image = canvas
        Next
        'ポン、チー、カン
        Dim NakiCount = 0
        Dim kancount = 0
        For i = 0 To 3
            Dim x = Players(0).PonHai(i).Hai
            For j = 0 To 2
                Dim idx = 13 - NakiCount * 3 - j
                If x <> 255 Then
                    Box(idx).Width = w
                    Box(idx).Height = w * 1.5
                    Box(idx).Top = Me.ClientSize.Height - w * 1.5
                    Box(idx).Left = Me.Width - w * (j + 1) - 16 - (w * 4 + 6) * NakiCount
                    Dim canvas As New Bitmap(PictureBox1.Width, PictureBox1.Height)
                    'ImageオブジェクトのGraphicsオブジェクトを作成する
                    DrawHai(canvas, x, Box(idx))
                    Box(idx).Image = canvas
                End If
            Next
            If x <> 255 Then
                NakiCount = NakiCount + 1
            End If
            x = 255
            For j = 0 To 2
                x = Players(0).CheeHai(i).Hai(j)
                Dim idx = 13 - NakiCount * 3 - j
                If x <> 255 Then
                    Box(idx).Width = w
                    Box(idx).Height = w * 1.5
                    Box(idx).Top = Me.ClientSize.Height - w * 1.5
                    Box(idx).Left = Me.Width - w * (j + 1) - 16 - (w * 4 + 6) * NakiCount
                    Dim canvas As New Bitmap(PictureBox1.Width, PictureBox1.Height)
                    'ImageオブジェクトのGraphicsオブジェクトを作成する
                    DrawHai(canvas, x, Box(idx))
                    Box(idx).Image = canvas
                End If
            Next
            If x <> 255 Then
                NakiCount = NakiCount + 1
            End If
            x = Players(0).KanHai(i).Hai
            For j = 0 To 2
                Dim idx = 13 - NakiCount * 3 - j
                If x <> 255 Then
                    Box(idx).Width = w
                    Box(idx).Height = w * 1.5
                    Box(idx).Top = Me.ClientSize.Height - w * 1.5
                    Box(idx).Left = Me.Width - w * (j + 1) - 16 - (w * 4 + 6) * NakiCount
                    Dim canvas As New Bitmap(PictureBox1.Width, PictureBox1.Height)
                    'ImageオブジェクトのGraphicsオブジェクトを作成する
                    DrawHai(canvas, x, Box(idx))
                    Box(idx).Image = canvas
                End If
            Next
            If x <> 255 Then
                Dim j = 3
                Dim idx = kancount
                PicKan(idx).Width = w
                PicKan(idx).Height = w * 1.5
                PicKan(idx).Top = Me.ClientSize.Height - w * 1.5
                PicKan(idx).Left = Me.Width - w * (j + 1) - 16 - (w * 4 + 6) * NakiCount
                Dim canvas As New Bitmap(PictureBox1.Width, PictureBox1.Height)
                'ImageオブジェクトのGraphicsオブジェクトを作成する
                DrawHai(canvas, x, PicKan(idx))
                PicKan(idx).Image = canvas
                PicKan(idx).Visible = True
                kancount = kancount + 1
            End If
            If x <> 255 Then
                NakiCount = NakiCount + 1
            End If
        Next
        For i = kancount To 15
            PicKan(i).Visible = False
        Next

        For i = 0 To 13
            Dim idx = i + 28
            Dim x = 34
            If Players(1).Tehai(i) = 255 Then
                x = 255
            End If
            If AgariPlayer = 1 Or ENEMY_VISIBLE Then
                x = Players(1).Tehai(i)
            End If
            PicEnemy(idx).Width = w * 1.5
            PicEnemy(idx).Height = w
            PicEnemy(idx).Left = Me.Width - w * 1.5 - 12
            PicEnemy(idx).Top = Me.Width - (i * w + 12 + Me.Width * 0.1) - w * 2
            If i = 13 Then
                PicEnemy(idx).Top = PicEnemy(idx).Top - 12
            End If
            Dim canvas As New Bitmap(PictureBox1.Width, PictureBox1.Height)
            DrawHai(canvas, x, PicEnemy(i))
            canvas.RotateFlip(RotateFlipType.Rotate270FlipNone)
            PicEnemy(idx).Image = canvas
        Next
        For i = 0 To 23
            Dim idx = i + 24
            Dim x = Sutehai(idx)
            If flgFirst Then
                PicSute(idx) = New System.Windows.Forms.PictureBox()
                Me.Controls.Add(PicSute(idx))
            End If
            PicSute(idx).Width = w * 1.5
            PicSute(idx).Height = w
            If i < 18 Then
                PicSute(idx).Top = Me.Width - ((Me.Width / 2 - 3 * w) + (i Mod 6) * w) - w - 12
                PicSute(idx).Left = Me.Width / 2 + 3 * w + Int(i / 6) * w * 1.5 + w * 0.5
            Else
                PicSute(idx).Top = Me.Width - ((Me.Width / 2 - 3 * w) + (i Mod 6) * w) - w - w * 6 - 12
                PicSute(idx).Left = Me.Width / 2 + 3 * w + Int(i / 6) * w * 1.5 + w * 0.5 + w * 1.5
            End If
            Dim canvas As New Bitmap(PictureBox1.Width, PictureBox1.Height)
            'ImageオブジェクトのGraphicsオブジェクトを作成する
            DrawHai(canvas, x, PicEnemy(0))
            canvas.RotateFlip(RotateFlipType.Rotate270FlipNone)
            PicSute(idx).Image = canvas
        Next
        For i = 0 To 13
            Dim x = 34
            If Players(2).Tehai(i) = 255 Then
                x = 255
            End If
            If AgariPlayer = 2 Or ENEMY_VISIBLE Then
                x = Players(2).Tehai(i)
            End If
            PicEnemy(i).Width = w
            PicEnemy(i).Height = w * 1.5
            PicEnemy(i).Left = Me.Width - (i * w + 24) - w - Me.Width * 0.1
            If i = 13 Then
                PicEnemy(i).Left = PicEnemy(i).Left - 12
            End If
            Dim canvas As New Bitmap(PictureBox1.Width, PictureBox1.Height)
            DrawHai(canvas, x, PicEnemy(i))
            canvas.RotateFlip(RotateFlipType.Rotate180FlipNone)
            PicEnemy(i).Image = canvas
        Next
        For i = 0 To 23
            Dim idx = i + 48
            Dim x = Sutehai(idx)
            If flgFirst Then
                PicSute(idx) = New System.Windows.Forms.PictureBox()
                Me.Controls.Add(PicSute(idx))
            End If
            PicSute(idx).Width = w
            PicSute(idx).Height = w * 1.5
            If i < 18 Then
                PicSute(idx).Left = Me.Width - ((Me.Width / 2 - 3 * w) + (i Mod 6) * w) - w + 6
                PicSute(idx).Top = Me.Width - (Me.Width / 2 + 3 * w + Int(i / 6) * w * 1.5) - w * 2 - 24
            Else
                PicSute(idx).Left = Me.Width - ((Me.Width / 2 - 3 * w) + (i Mod 6) * w) - w + 6 - w * 6
                PicSute(idx).Top = Me.Width - (Me.Width / 2 + 3 * w + Int(i / 6) * w * 1.5) - w * 2 + w * 1.5 - 24
            End If
            Dim canvas As New Bitmap(PictureBox1.Width, PictureBox1.Height)
            'ImageオブジェクトのGraphicsオブジェクトを作成する
            DrawHai(canvas, x, PicSute(idx))
            canvas.RotateFlip(RotateFlipType.Rotate180FlipNone)
            PicSute(idx).Image = canvas
        Next
        For i = 0 To 13
            Dim idx = i + 14
            Dim x = 34
            If Players(3).Tehai(i) = 255 Then
                x = 255
            End If
            If AgariPlayer = 3 Or ENEMY_VISIBLE Then
                x = Players(3).Tehai(i)
            End If
            PicEnemy(idx).Width = w * 1.5
            PicEnemy(idx).Height = w
            PicEnemy(idx).Left = 0
            PicEnemy(idx).Top = i * w + 12 + Me.Width * 0.1
            If i = 13 Then
                PicEnemy(idx).Top = PicEnemy(idx).Top + 12
            End If
            Dim canvas As New Bitmap(PictureBox1.Width, PictureBox1.Height)
            DrawHai(canvas, x, PicEnemy(i))
            canvas.RotateFlip(RotateFlipType.Rotate90FlipNone)
            PicEnemy(idx).Image = canvas
        Next
        For i = 0 To 23
            Dim idx = i + 72
            Dim x = Sutehai(idx)
            If flgFirst Then
                PicSute(idx) = New System.Windows.Forms.PictureBox()
                Me.Controls.Add(PicSute(idx))
            End If
            PicSute(idx).Width = w * 1.5
            PicSute(idx).Height = w
            If i < 18 Then
                PicSute(idx).Top = (Me.Width / 2 - 3 * w) + (i Mod 6) * w - 12
                PicSute(idx).Left = Me.Width - (Me.Width / 2 + 3 * w + Int(i / 6) * w * 1.5) - w * 2.5
            Else
                PicSute(idx).Top = (Me.Width / 2 - 3 * w) + (i Mod 6) * w + w * 6 - 12
                PicSute(idx).Left = Me.Width - (Me.Width / 2 + 3 * w + Int(i / 6) * w * 1.5) - w * 2.5 + w * 1.5
            End If
            Dim canvas As New Bitmap(PictureBox1.Width, PictureBox1.Height)
            'ImageオブジェクトのGraphicsオブジェクトを作成する
            DrawHai(canvas, x, PicEnemy(0))
            canvas.RotateFlip(RotateFlipType.Rotate90FlipNone)
            PicSute(idx).Image = canvas
        Next
        flgFirst = False
        ResumeLayout()
        Me.Update()
    End Sub

    Private Sub Form1_Resize(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Resize
        Height = Width
        Draw()
    End Sub
    Private Function Think(ByVal p As Integer) As Integer
        Dim th As Array = Players(p).Tehai.Clone
        Return 12
    End Function
    Class Mentsu
        Public Hai(4) As Integer '0-8:ソーズ 9-17:ピンズ 18-26:マンズ 27-33:字牌 34:裏面 255:無し
        Public Naki As Boolean 'True:鳴き False:面前
        Public Sub New()
            For i = 0 To 3
                Hai(i) = 255
            Next
            Naki = False
        End Sub
        'メンツが出来ているか
        Public Function IsExist() As Boolean
            Return (Hai(0) <> 255)
        End Function
        '同じ牌の枚数を返す
        Public Function GetSameNum() As Integer
            Dim c = 1
            For i = 1 To 3
                If Hai(0) <> 255 And Hai(0) = Hai(i) Then
                    c = c + 1
                End If
            Next
            Return c
        End Function
    End Class
    Private Function IsDora(ByVal h As Integer) As Boolean
        For i = 0 To 4
            Dim x = 255
            If i <= Players(0).GetKanCount Then
                x = YamaHai(4 + i * 2)
            End If
            Dim buff = x + 1
            If buff = 9 Then
                buff = 0
            End If
            If buff = 18 Then
                buff = 9
            End If
            If buff = 27 Then
                buff = 18
            End If
            If buff = 31 Then
                buff = 27
            End If
            If buff = 34 Then
                buff = 31
            End If
            If h = buff Then
                Return True
            End If
        Next
        Return False
    End Function
    Private Function AgariHantei(ByRef p As Player) As Boolean
        'メンツの準備
        Dim m(4) As Mentsu
        Dim n = 0
        For n = 0 To 3
            m(n) = New Mentsu
        Next
        '鳴きの計算
        n = 0
        For i = 0 To 3
            If p.PonHai(i).Hai <> 255 Then
                For j = 0 To 2
                    m(n).Hai(j) = p.PonHai(i).Hai
                Next
                m(n).Naki = True
                n = n + 1
            End If
            If p.KanHai(i).Hai <> 255 Then
                For j = 0 To 3
                    m(n).Hai(j) = p.KanHai(i).Hai
                Next
                m(n).Naki = True
                n = n + 1
            End If
            If p.CheeHai(i).Hai(0) <> 255 Then
                Dim min = 99
                For j = 0 To 2
                    If p.CheeHai(i).Hai(j) < min Then
                        min = p.CheeHai(i).Hai(j)
                    End If
                Next
                For j = 0 To 2
                    m(n).Hai(j) = min + j
                Next
                m(n).Naki = True
                n = n + 1
            End If
        Next
        '手牌のコピー
        Dim TumoFlag As Boolean = True
        Dim th As Array = p.Tehai.Clone
        If th(p.GetTumoIdx()) = 255 Then
            TumoFlag = False
            th(p.GetTumoIdx()) = NakiHai
        End If
        Dim AgariHai = th(p.GetTumoIdx())
        System.Array.Sort(th)
        Dim buff As Array = th.Clone
        '上がり判定と点計算
        Dim max = 0
        For i = 0 To p.GetTumoIdx() - 1
            th = buff.Clone
            Dim j = i + 1
            If th(i) = th(j) Then
                '雀頭とみなす
                Dim a = th(i)
                th(i) = 255
                th(j) = 255
                '面子の数
                Dim k = n
                '刻子があるか
                For x = 0 To 13
                    If th(x) = 255 Then
                        Continue For
                    Else
                        For y = x + 1 To 13
                            If th(y) = 255 Then
                                Continue For
                            ElseIf th(y) = th(x) Then
                                For z = y + 1 To 13
                                    If th(z) = 255 Then
                                        Continue For
                                    ElseIf th(z) = th(x) Then
                                        m(k).Hai(0) = th(x)
                                        m(k).Hai(1) = th(y)
                                        m(k).Hai(2) = th(z)
                                        m(k).Naki = False
                                        k = k + 1
                                        th(x) = 255
                                        th(y) = 255
                                        th(z) = 255
                                    End If
                                Next
                            End If
                        Next
                    End If
                Next
                '順子があるか
                For x = 0 To 13
                    If th(x) = 255 Then
                        Continue For
                    Else
                        Dim findflag = False
                        For y = x + 1 To 13
                            If IsSameCode(th(x), th(y)) And th(x) + 1 = th(y) Then
                                For z = y + 1 To 13
                                    If IsSameCode(th(y), th(z)) And th(y) + 1 = th(z) Then
                                        m(k).Hai(0) = th(x)
                                        m(k).Hai(1) = th(y)
                                        m(k).Hai(2) = th(z)
                                        '順子はソートしておく
                                        Dim min = 99
                                        For j = 0 To 2
                                            If m(k).Hai(j) < min Then
                                                min = m(k).Hai(j)
                                            End If
                                        Next
                                        For j = 0 To 2
                                            m(k).Hai(j) = min + j
                                        Next
                                        m(k).Naki = False
                                        k = k + 1
                                        th(x) = 255
                                        th(y) = 255
                                        th(z) = 255
                                        findflag = True
                                    End If
                                    If findflag Then
                                        Exit For
                                    End If
                                Next
                            End If
                            If findflag Then
                                Exit For
                            End If
                        Next
                    End If
                Next
                If k = 4 Then
                    Dim buf = YakuHantei(p, AgariHai, TumoFlag, a, m)
                    If max < buf Then
                        max = buf
                    End If
                End If
            End If
        Next
        For i = 0 To p.GetTumoIdx() - 1
            th = buff.Clone
            Dim j = i + 1
            If th(i) = th(j) Then
                '雀頭とみなす
                Dim a = th(i)
                th(i) = 255
                th(j) = 255
                '七対子の判定
                Dim t = 1
                For x = 0 To 12
                    If th(x) = 255 Then
                        Continue For
                    Else
                        Dim y = x + 1
                        If th(y) = th(x) Then
                            th(x) = 255
                            th(y) = 255
                            t = t + 1
                        End If
                    End If
                Next
                If t = 7 Then
                    Dim hu = 25
                    Dim han = 2
                    '親はイーハン上がり
                    If 27 = p.Kaze And 0 < han Then
                        han = han + 1
                    End If
                    '点計算
                    Dim buf = TenKeisan(hu, han)
                    If max < buf Then
                        Yaku = "七対子" + vbCrLf + buf.ToString + "点"
                        max = buf
                    End If
                End If
            End If
        Next
        If max > 0 Then
            AgariTen = max
            Return True
        End If
        Return False
    End Function
    Private Function YakuHantei(ByRef p As Player, ByVal agarihai As Integer, ByVal tumoflag As Boolean, ByVal a As Integer, ByRef m() As Mentsu) As Integer
        Dim hu = 20
        Dim han = 0
        Yaku = ""
        Dim menzen = True
        For i = 0 To 3
            If m(i).Naki = True Then
                menzen = False
            End If
        Next
        '平和の判定
        Dim flag = menzen
        For i = 0 To 3
            If m(i).GetSameNum > 1 Then
                flag = False
            End If
        Next
        Dim ryanmen = False
        If IsSameCode(agarihai, agarihai - 3) Then
            For i = 0 To 3
                If m(i).Hai(0) = agarihai - 2 And m(i).Hai(1) = agarihai - 1 Then
                    ryanmen = True
                End If
            Next
        End If
        If IsSameCode(agarihai, agarihai + 3) Then
            For i = 0 To 3
                If m(i).Hai(1) = agarihai + 1 And m(i).Hai(2) = agarihai + 2 Then
                    ryanmen = True
                End If
            Next
        End If
        If flag And ryanmen Then
            Yaku = Yaku + "平和" + vbCrLf
            han = han + 1
        End If
        '面前自摸
        If menzen And tumoflag Then
            Yaku = Yaku + "面前自摸" + vbCrLf
            han = han + 1
        End If
        'タンヤオ
        Dim t = True
        If IsRoutouHai(a) Then
            t = False
        End If
        For i = 0 To 3
            For j = 0 To 2
                If IsRoutouHai(m(i).Hai(j)) Then
                    t = False
                End If
            Next
        Next
        If t Then
            Yaku = Yaku + "断公" + vbCrLf
            han = han + 1
        End If
        '一盃口
        Dim ipk = 0
        For i = 0 To 3
            For j = i + 1 To 3
                If m(j).Hai(0) = m(i).Hai(0) And m(j).Hai(1) = m(i).Hai(1) And m(j).Hai(2) = m(i).Hai(2) Then
                    ipk = ipk + 1
                    Exit For
                End If
            Next
        Next
        If ipk = 1 And menzen Then
            Yaku = Yaku + "一盃口" + vbCrLf
            han = han + 1
        End If
        If ipk = 2 And menzen Then
            Yaku = Yaku + "二盃口" + vbCrLf
            han = han + 3
        End If
        If ipk = 2 And menzen = False Then
            Yaku = Yaku + "二盃口（鳴き）" + vbCrLf
            han = han + 2
        End If
        '白発中
        For i = 0 To 3
            If m(i).Hai(0) = 31 Then
                Yaku = Yaku + "白" + vbCrLf
                han = han + 1
            End If
            If m(i).Hai(0) = 32 Then
                Yaku = Yaku + "発" + vbCrLf
                han = han + 1
            End If
            If m(i).Hai(0) = 33 Then
                Yaku = Yaku + "中" + vbCrLf
                han = han + 1
            End If
        Next
        '風牌
        For i = 0 To 3
            If p.Kaze = BaKaze And m(i).Hai(0) = p.Kaze Then
                Yaku = Yaku + "連風牌" + vbCrLf
                han = han + 2
            ElseIf m(i).Hai(0) = BaKaze Then
                Yaku = Yaku + GetKazeName(BaKaze) + vbCrLf
                han = han + 1
            ElseIf m(i).Hai(0) = p.Kaze Then
                Yaku = Yaku + GetKazeName(p.Kaze) + vbCrLf
                han = han + 1
            End If
        Next
        '海底
        If TumoCount = 136 And tumoflag Then
            Yaku = Yaku + "海底摸月" + vbCrLf
            han = han + 1
        End If
        If TumoCount = 136 And False = tumoflag Then
            Yaku = Yaku + "河底𢭐魚" + vbCrLf
            han = han + 1
        End If
        '三色同順
        flag = False
        For i = 1 To 3
            If m(i).GetSameNum() = 1 And IsSameCode(0, m(i).Hai(0)) Then
                'ソーズと残りを比較
                Dim pinz = False
                Dim manz = False
                For j = 0 To 3
                    If m(j).GetSameNum() = 1 And m(j).Hai(0) = m(i).Hai(0) + 9 Then
                        pinz = True
                    End If
                    If m(j).GetSameNum() = 1 And m(j).Hai(0) = m(i).Hai(0) + 18 Then
                        manz = True
                    End If
                Next
                If pinz And manz Then
                    flag = True
                End If
            End If
        Next
        If flag Then
            If menzen Then
                Yaku = Yaku + "三色同順" + vbCrLf
                han = han + 2
            Else
                Yaku = Yaku + "三色同順（鳴）" + vbCrLf
                han = han + 1
            End If
        End If
        '対々和
        flag = True
        For i = 0 To 3
            If m(i).GetSameNum = 1 Then
                flag = False
            End If
        Next
        If flag Then
            Yaku = Yaku + "対々和" + vbCrLf
            han = han + 2
        End If
        '一気通貫
        flag = False
        For code = 0 To 18 Step 9
            Dim f123 = False
            Dim f456 = False
            Dim f789 = False
            For i = 1 To 3
                If m(i).Hai(0) = 0 + code And m(i).Hai(1) = 1 + code And m(i).Hai(2) = 2 + code Then
                    f123 = True
                End If
                If m(i).Hai(0) = 3 + code And m(i).Hai(1) = 4 + code And m(i).Hai(2) = 5 + code Then
                    f456 = True
                End If
                If m(i).Hai(0) = 6 + code And m(i).Hai(1) = 7 + code And m(i).Hai(2) = 8 + code Then
                    f789 = True
                End If
            Next
            If f123 And f456 And f789 Then
                flag = True
            End If
        Next
        If flag Then
            If menzen Then
                Yaku = Yaku + "一気通貫" + vbCrLf
                han = han + 2
            Else
                Yaku = Yaku + "一気通貫（鳴）" + vbCrLf
                han = han + 1
            End If
        End If
        '三暗刻、四暗刻
        Dim count = 0
        For i = 0 To 3
            If 3 <= m(i).GetSameNum And m(i).Naki = False Then
                count = count + 1
            End If
        Next
        If count = 3 Then
            Yaku = Yaku + "三暗刻" + vbCrLf
            han = han + 2
        End If
        If count = 4 Then
            Yaku = Yaku + "四暗刻" + vbCrLf
            han = han + 13
        End If
        '大三元
        count = 0
        For i = 0 To 3
            If m(i).Hai(0) = 31 Then
                count = count = 1
            End If
            If m(i).Hai(0) = 32 Then
                count = count = 1
            End If
            If m(i).Hai(0) = 33 Then
                count = count = 1
            End If
        Next
        If count = 3 Then
            Yaku = Yaku + "大三元" + vbCrLf
            han = han + 13
        End If
        'チャンタ
        flag = True
        If False = IsRoutouHai(a) Then
            flag = False
        End If
        For i = 0 To 3
            Dim routou = False
            For j = 0 To 2
                If IsRoutouHai(m(i).Hai(j)) Then
                    routou = True
                End If
            Next
            If routou = False Then
                flag = False
            End If
        Next
        Dim jihai = False
        If IsJihai(a) Then
            jihai = True
        End If
        For i = 0 To 3
            For j = 0 To 2
                If IsJihai(m(i).Hai(j)) Then
                    jihai = True
                End If
            Next
        Next
        If flag And jihai And menzen Then
            Yaku = Yaku + "全帯" + vbCrLf
            han = han + 2
        End If
        If flag And jihai And False = menzen Then
            Yaku = Yaku + "全帯（鳴き）" + vbCrLf
            han = han + 1
        End If
        If flag And False = jihai And menzen Then
            Yaku = Yaku + "純全帯" + vbCrLf
            han = han + 3
        End If
        If flag And False = jihai And False = menzen Then
            Yaku = Yaku + "純全帯（鳴き）" + vbCrLf
            han = han + 2
        End If
        '混一色
        jihai = False
        Dim suhai = True
        Dim sample = 255
        If IsJihai(a) Then
            jihai = True
        Else
            sample = a
        End If
        For i = 0 To 3
            If IsJihai(m(i).Hai(0)) Then
                jihai = True
            ElseIf sample = 255 Then
                sample = m(i).Hai(0)
            ElseIf Not IsSameCode(sample, m(i).Hai(0)) Then
                suhai = False
            End If
        Next
        flag = jihai And suhai
        If flag And menzen Then
            Yaku = Yaku + "混一色" + vbCrLf
            han = han + 3
        End If
        If flag And False = menzen Then
            Yaku = Yaku + "混一色（鳴き）" + vbCrLf
            han = han + 2
        End If
        '清一色
        flag = True
        For i = 0 To 3
            If Not IsSameCode(a, m(i).Hai(0)) Then
                flag = False
            End If
        Next
        If flag Then
            Yaku = Yaku + "清一色" + vbCrLf
            han = han + 6
        End If
        '字一色
        flag = True
        If Not IsJihai(a) Then
            flag = False
        End If
        For i = 0 To 3
            If Not IsJihai(m(i).Hai(0)) Then
                flag = False
            End If
        Next
        If flag Then
            Yaku = Yaku + "字一色" + vbCrLf
            han = han + 13
        End If
        'ドラ
        Dim d = 0
        If IsDora(a) Then
            d = d + 2
        End If
        For i = 0 To 3
            For j = 0 To 3
                If IsDora(m(i).Hai(j)) Then
                    d = d + 1
                End If
            Next
        Next
        If 0 < d And 0 < han Then
            Yaku = Yaku + "ドラ" + d.ToString + vbCrLf
            han = han + d
        End If
        '親はイーハン上がり
        If 27 = p.Kaze And 0 < han Then
            han = han + 1
        End If
        '点計算
        Dim ret = TenKeisan(hu, han)
        If ret = 10 Then
            Yaku = "役なし"
        Else
            Yaku = Yaku + ret.ToString + "点"
        End If
        Return ret
    End Function
    Private Function IsRoutouHai(ByVal x As Integer) As Boolean
        If x <> 255 And (x = 0 Or x = 8 Or x = 9 Or x = 17 Or x = 18 Or 26 <= x) Then
            Return True
        End If
        Return False
    End Function
    Private Function IsJihai(ByVal x As Integer) As Boolean
        If x <> 255 And 27 <= x Then
            Return True
        End If
        Return False
    End Function
    Private Function GetKazeName(ByVal i As Integer) As String
        Dim ret As String = ""
        Select Case i
            Case 27
                ret = "東"
            Case 28
                ret = "南"
            Case 29
                ret = "西"
            Case 30
                ret = "北"
        End Select
        Return ret
    End Function
    Private Function TenKeisan(ByVal hu As Integer, ByVal han As Integer) As Integer
        Dim score = 0
        Select Case han
            Case 0
                score = 10
            Case 1
                score = 1000
            Case 2
                score = 2000
            Case 3
                score = 3900
            Case 4
                score = 7700
            Case 5
                score = 8000
            Case 6, 7
                score = 12000
            Case 8, 9, 10
                score = 18000
            Case 11, 12
                score = 24000
            Case Else
                score = 32000
        End Select
        Return score
    End Function
    Private Function PonHantei()
        Dim count = 1
        For i = 0 To Players(0).GetTumoIdx()
            If Players(0).Tehai(i) = NakiHai Then
                count = count + 1
            End If
        Next
        If count >= 3 Then
            Return True
        End If
        Return False
    End Function
    Private Function MinKanHantei()
        Dim count = 1
        For i = 0 To Players(0).GetTumoIdx()
            If Players(0).Tehai(i) = NakiHai Then
                count = count + 1
            End If
        Next
        If count = 4 Then
            Return True
        End If
        Return False
    End Function
    Private Function AnKanHantei() As Boolean
        Dim count = 0
        For i = 0 To Players(0).GetTumoIdx()
            count = 1
            For j = i + 1 To Players(0).GetTumoIdx()
                If Players(0).Tehai(i) = Players(0).Tehai(j) Then
                    count = count + 1
                End If
            Next
            If count = 4 Then
                Return True
            End If
        Next
        Return False
    End Function
    Private Function IsSameCode(ByVal a As Integer, ByVal b As Integer)
        If 0 <= a And a <= 8 Then
            If 0 <= b And b <= 8 Then
                Return True
            End If
        End If
        If 9 <= a And a <= 17 Then
            If 9 <= b And b <= 17 Then
                Return True
            End If
        End If
        If 18 <= a And a <= 26 Then
            If 18 <= b And b <= 26 Then
                Return True
            End If
        End If
        Return False
    End Function
    Private Function CheeHantei()
        Dim ret = False
        For i = 0 To 6
            CheeSelect(i) = 255
        Next
        Dim flgA = False
        Dim flgB = False
        Dim n = NakiHai - 2
        Dim m = NakiHai - 1
        For i = 0 To Players(0).GetTumoIdx()
            'down
            If IsSameCode(n, NakiHai) And n = Players(0).Tehai(i) Then
                flgA = True
            End If
            If IsSameCode(m, NakiHai) And m = Players(0).Tehai(i) Then
                flgB = True
            End If
        Next
        If flgA And flgB Then
            CheeSelect(0) = m
            CheeSelect(1) = n
            ret = True
        End If
        flgA = False
        flgB = False
        n = NakiHai - 1
        m = NakiHai + 1
        For i = 0 To Players(0).GetTumoIdx()
            'mid
            If IsSameCode(n, NakiHai) And n = Players(0).Tehai(i) Then
                flgA = True
            End If
            If IsSameCode(m, NakiHai) And m = Players(0).Tehai(i) Then
                flgB = True
            End If
        Next
        If flgA And flgB Then
            CheeSelect(2) = m
            CheeSelect(3) = n
            ret = True
        End If
        flgA = False
        flgB = False
        n = NakiHai + 1
        m = NakiHai + 2
        For i = 0 To Players(0).GetTumoIdx()
            'mid
            If IsSameCode(n, NakiHai) And n = Players(0).Tehai(i) Then
                flgA = True
            End If
            If IsSameCode(m, NakiHai) And m = Players(0).Tehai(i) Then
                flgB = True
            End If
        Next
        If flgA And flgB Then
            CheeSelect(4) = m
            CheeSelect(5) = n
            ret = True
        End If
        Return ret
    End Function
    Private Function NakiHantei()
        If NakiHai = 255 Then
            Return False
        End If
        Dim pon = False
        Dim chee = False
        Dim kan = False
        Dim ron = False
        Dim ret = False
        pon = PonHantei()
        chee = CheeHantei()
        kan = MinKanHantei()
        ron = AgariHantei(Players(0))
        If pon Then
            If Turn <> 0 Then
                ret = True
                ButtonPon.Visible = True
                ButtonCancel.Visible = True
                ClickLock = True
            End If
        End If
        If chee Then
            If Turn = 3 Then
                ret = True
                ButtonChee.Visible = True
                ButtonCancel.Visible = True
                ClickLock = True
            End If
        End If
        If kan Then
            If Turn <> 0 Then
                ret = True
                ButtonKan.Visible = True
                ButtonCancel.Visible = True
                ClickLock = True
            End If
        End If
        If ron Then
            If Turn <> 0 Then
                ret = True
                RonPlayer = Turn
                ButtonTumo.Text = "ロン"
                ButtonTumo.Visible = True
                ButtonCancel.Visible = True
                ClickLock = True
            End If
        End If
        ButtonSort()
        Draw()
        Return ret
    End Function
    Dim RonPlayer As Integer = 0
    Private Sub SuteHaiSub(ByVal idx As Integer, ByRef st() As Integer, ByRef p As Player)
        st(p.SuteIdx + Turn * 24) = p.Tehai(idx)
        NakiHai = p.Tehai(idx)
        p.Tehai(idx) = p.Tehai(p.GetTumoIdx())
        p.Tehai(p.GetTumoIdx()) = 255
        Sort(p.Tehai)
        p.SuteIdx = p.SuteIdx + 1
        Draw()
    End Sub
    Private Sub OyaNext()
        '場風を進める
        BaTurn = BaTurn + 1
        BaKaze = (Int(BaTurn / 4) Mod 4 + 27)
        '親を進める()
        Oya = Oya + 1
        If Oya = 4 Then
            Oya = 0
        End If
        '門風を進める
        Dim Ton = Oya
        If Ton = 0 Then
            Players(0).Kaze = 27
            Players(1).Kaze = 28
            Players(2).Kaze = 29
            Players(3).Kaze = 30
        End If
        If Ton = 1 Then
            Players(0).Kaze = 30
            Players(1).Kaze = 27
            Players(2).Kaze = 28
            Players(3).Kaze = 29
        End If
        If Ton = 2 Then
            Players(0).Kaze = 29
            Players(1).Kaze = 30
            Players(2).Kaze = 27
            Players(3).Kaze = 28
        End If
        If Ton = 3 Then
            Players(0).Kaze = 28
            Players(1).Kaze = 29
            Players(2).Kaze = 30
            Players(3).Kaze = 27
        End If
    End Sub
    Private Sub TurnNext()
        Turn = Turn + 1
        If Turn = 4 Then
            Turn = 0
        End If
    End Sub
    Private Function Tumo(ByRef p As Player) As Boolean
        If TumoCount = 136 Then
            '流局
            MsgBox("流局")
            If Oya <> Turn - 1 Then
                OyaNext()
            End If
            FirstLoop()
            Draw()
            Return False
        End If
        p.Tehai(p.GetTumoIdx()) = YamaHai(TumoCount)
        TumoCount = TumoCount + 1
        Draw()
        Return True
    End Function
    Private Function CheeExecute(ByVal idx As Integer)
        If idx = 255 Then
            Return False
        End If
        Dim x = Players(0).Tehai(idx)
        Dim a = 255
        Dim b = 255
        Dim c = 255
        If CheeSelect(0) = x Or CheeSelect(1) = x Then
            a = CheeSelect(0)
            b = CheeSelect(1)
            c = NakiHai
        ElseIf CheeSelect(2) = x Or CheeSelect(3) = x Then
            a = CheeSelect(2)
            b = CheeSelect(3)
            c = NakiHai
        ElseIf CheeSelect(4) = x Or CheeSelect(5) = x Then
            a = CheeSelect(4)
            b = CheeSelect(5)
            c = NakiHai
        Else
            Return False
        End If
        For x = 0 To Players(0).GetTumoIdx
            If Players(0).Tehai(x) = a Then
                Players(0).Tehai(x) = 255
                Exit For
            End If
        Next
        For x = 0 To Players(0).GetTumoIdx
            If Players(0).Tehai(x) = b Then
                Players(0).Tehai(x) = 255
                Exit For
            End If
        Next
        Sort(Players(0).Tehai)
        For i = 0 To 3
            If Players(0).CheeHai(i).Hai(0) = 255 Then
                Players(0).CheeHai(i).Hai(0) = a
                Players(0).CheeHai(i).Hai(1) = b
                Players(0).CheeHai(i).Hai(2) = c
                Players(0).CheeHai(i).Houi = Turn
                Exit For
            End If
        Next
        NakiHaiKeshi()
        Draw()
        Turn = 0
        ButtonInvisible()
        ClickLock = False
        Enabled = True
        Return True
    End Function
    Private Sub NakiHaiKeshi()
        Dim buff = Turn - 1
        If buff = -1 Then
            buff = 3
        End If
        Players(buff).SuteIdx = Players(buff).SuteIdx - 1
        Sutehai(Players(buff).SuteIdx + buff * 24) = 255
        NakiHai = 255
    End Sub
    Private Sub ClickEvent(ByVal idx As Integer)
        Enabled = False
        StartFlag = False
        If CheeSelectMode Then
            If CheeExecute(idx) Then
                CheeSelectMode = False
                ClickLock = False
                Exit Sub
            Else
                Enabled = True
                Exit Sub
            End If
        End If
        If Turn = 0 And ClickLock = False Then
            ClickLock = True
            If idx <> 255 Then
                SuteHaiSub(idx, Sutehai, Players(0))
                ButtonInvisible()
                ButtonFlag = 0
            Else
                Dim degub = idx
            End If
            If NakiHantei() Then
                Enabled = True
                Exit Sub
            Else
                TurnNext()
            End If
        Else
            Select Case ButtonFlag
                Case 0 ' しない
                    ButtonInvisible()
                Case 1
                    'MsgBox("ポン")
                    For i = 0 To 3
                        If Players(0).PonHai(i).Hai = 255 Then
                            Dim pondel = 2
                            For x = 0 To Players(0).GetTumoIdx
                                If Players(0).Tehai(x) = NakiHai And pondel > 0 Then
                                    Players(0).Tehai(x) = 255
                                    pondel = pondel - 1
                                End If
                            Next
                            Sort(Players(0).Tehai)
                            Players(0).PonHai(i).Hai = NakiHai
                            Players(0).PonHai(i).Houi = Turn
                            NakiHaiKeshi()
                            Draw()
                            Turn = 0
                            ButtonInvisible()
                            ClickLock = False
                            Enabled = True
                            Exit Sub
                        End If
                    Next
                    ClickLock = False
                Case 2
                    'MsgBox("チー")
                    CheeSelectMode = True
                    Draw()
                    Turn = 0
                    ButtonInvisible()
                    Enabled = True
                    Exit Sub
                Case 3
                    'MsgBox("Kan")
                    For i = 0 To 3
                        If Players(0).KanHai(i).Hai = 255 Then
                            If Turn <> 1 Then
                                'ミンカン
                                Dim kandel = 3
                                For x = 0 To Players(0).GetTumoIdx()
                                    If Players(0).Tehai(x) = NakiHai And kandel > 0 Then
                                        Players(0).Tehai(x) = 255
                                        kandel = kandel - 1
                                    End If
                                Next
                            Else
                                'アンカン
                                For x = 0 To Players(0).GetTumoIdx()
                                    Dim count = 1
                                    For y = x + 1 To Players(0).GetTumoIdx()
                                        If Players(0).Tehai(x) = Players(0).Tehai(y) Then
                                            count = count + 1
                                        End If
                                    Next
                                    If count = 4 Then
                                        NakiHai = Players(0).Tehai(x)
                                    End If
                                Next
                                Dim kandel = 4
                                For x = 0 To Players(0).GetTumoIdx()
                                    If Players(0).Tehai(x) = NakiHai And kandel > 0 Then
                                        Players(0).Tehai(x) = 255
                                        kandel = kandel - 1
                                    End If
                                Next
                            End If
                            '捨てないソート
                            Dim buff As Array = Players(0).Tehai.Clone
                            System.Array.Sort(buff)
                            For n = 0 To 12
                                Players(0).Tehai(n) = buff(n)
                            Next
                            Players(0).KanHai(i).Hai = NakiHai
                            Players(0).KanHai(i).Houi = Turn
                            If Turn <> 1 Then
                                NakiHaiKeshi()
                            End If
                            Tumo(Players(0)) ' リンシャンツモ
                            Turn = 0
                            ButtonInvisible()
                            ClickLock = False
                            Enabled = True
                            Exit Sub
                        End If
                    Next
                    ClickLock = False
                Case 4
                    ButtonInvisible()
                    MsgBox(ButtonTumo.Text + vbCrLf + Yaku)
                    TenKoukan(0)
                    If Oya <> 0 Then
                        OyaNext()
                    End If
                    FirstLoop()
                    Draw()
                    ClickLock = False
                    Enabled = True
                    Exit Sub
                Case Else
                    ClickLock = True
                    Exit Sub
            End Select
        End If
        For i = 1 To 3
            If Turn = i Then
                If Tumo(Players(i)) = False Then
                    ClickLock = False
                    Enabled = True
                    Exit Sub
                End If
                If AgariHantei(Players(i)) Then
                    AgariPlayer = i
                    Draw()
                    MsgBox("ツモ" + vbCrLf + Yaku)
                    TenKoukan(i)
                    If Oya <> Turn Then
                        OyaNext()
                    End If
                    FirstLoop()
                    Draw()
                    ClickLock = False
                    Enabled = True
                    Exit Sub
                End If
                idx = Think(i) ' 思考ルーチン
                SuteHaiSub(idx, Sutehai, Players(i))
                If NakiHantei() Then
                    Enabled = True
                    Exit Sub
                Else
                    TurnNext()
                End If
            End If
        Next
        PlayerTumo()
        ClickLock = False
        Enabled = True
    End Sub
    Private Sub TenKoukan(ByVal ap As Integer)
        If AgariTen = 10 Then
        ElseIf RonPlayer <> ap Then
            Players(RonPlayer).Tokuten = Players(RonPlayer).Tokuten - AgariTen
            Players(ap).Tokuten = Players(ap).Tokuten + AgariTen
        ElseIf Players(ap).Kaze = 27 Then
            Dim harai = Math.Ceiling(AgariTen / 300) * 100
            For i = 0 To 3
                If i <> ap Then
                    Players(i).Tokuten = Players(i).Tokuten - harai / 3
                End If
            Next
            Players(ap).Tokuten = Players(ap).Tokuten + harai * 3
        Else
            Dim OyaBarai As Integer = 0
            Dim KoBarai As Integer = 0
            Select Case AgariTen
                Case 1000
                    OyaBarai = 500
                    KoBarai = 300
                Case 2000
                    OyaBarai = 1000
                    KoBarai = 500
                Case 3900
                    OyaBarai = 2000
                    KoBarai = 1000
                Case 7700
                    OyaBarai = 3900
                    KoBarai = 2000
                Case Else
                    OyaBarai = AgariTen / 2
                    KoBarai = AgariTen / 4
            End Select
            For i = 0 To 3
                If AgariPlayer = i Then
                    Players(0).Tokuten = Players(0).Tokuten + OyaBarai + KoBarai
                ElseIf Players(i).Kaze = 27 Then
                    Players(i).Tokuten = Players(i).Tokuten - OyaBarai
                Else
                    Players(i).Tokuten = Players(i).Tokuten - KoBarai
                End If
            Next
        End If
    End Sub
    Private Sub ButtonInvisible()
        ButtonPon.Visible = False
        ButtonChee.Visible = False
        ButtonKan.Visible = False
        ButtonTumo.Visible = False
        ButtonCancel.Visible = False
    End Sub
    Private Sub ButtonSort()
        Dim btn() As System.Windows.Forms.Button = {ButtonCancel, ButtonPon, ButtonChee, ButtonKan, ButtonTumo}
        Dim l = PicSute(5).Right
        For i = 0 To 4
            If btn(i).Visible = True Then
                btn(i).Left = l
                l = btn(i).Right + 1
            End If
        Next
    End Sub
    Private Sub PictureBox1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PictureBox1.Click
        ClickEvent(0)
    End Sub
    Private Sub PictureBox2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PictureBox2.Click
        ClickEvent(1)
    End Sub
    Private Sub PictureBox3_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PictureBox3.Click
        ClickEvent(2)
    End Sub
    Private Sub PictureBox4_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PictureBox4.Click
        ClickEvent(3)
    End Sub
    Private Sub PictureBox5_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PictureBox5.Click
        ClickEvent(4)
    End Sub
    Private Sub PictureBox6_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PictureBox6.Click
        ClickEvent(5)
    End Sub
    Private Sub PictureBox7_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PictureBox7.Click
        ClickEvent(6)
    End Sub
    Private Sub PictureBox8_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PictureBox8.Click
        ClickEvent(7)
    End Sub
    Private Sub PictureBox9_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PictureBox9.Click
        ClickEvent(8)
    End Sub
    Private Sub PictureBox10_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PictureBox10.Click
        ClickEvent(9)
    End Sub
    Private Sub PictureBox11_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PictureBox11.Click
        ClickEvent(10)
    End Sub
    Private Sub PictureBox12_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PictureBox12.Click
        ClickEvent(11)
    End Sub
    Private Sub PictureBox13_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PictureBox13.Click
        ClickEvent(12)
    End Sub
    Private Sub PictureBox14_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PictureBox14.Click
        ClickEvent(13)
    End Sub

    Private Sub ButtonCancel_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonCancel.Click
        TurnNext()
        ButtonFlag = 0
        ClickEvent(255)
    End Sub

    Private Sub ButtonPon_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonPon.Click
        TurnNext()
        ButtonFlag = 1
        ClickEvent(255)
    End Sub

    Private Sub ButtonChee_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonChee.Click
        TurnNext()
        ButtonFlag = 2
        ClickEvent(255)
    End Sub

    Private Sub ButtonKan_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonKan.Click
        TurnNext()
        ButtonFlag = 3
        ClickEvent(255)
    End Sub

    Private Sub ButtonTumo_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonTumo.Click
        TurnNext()
        ButtonFlag = 4
        ClickEvent(255)
    End Sub
    Private Sub Form1_KeyDown(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles Me.KeyDown
        If e.KeyCode = Keys.A Then
            'Form2.Show()
        End If
    End Sub
End Class
