Public Class Form2

    Private Sub Form2_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
    End Sub

    Private Sub Form2_Shown(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Shown
        Dim canvas As New Bitmap(Mahjong.Width, Mahjong.Height)
        Dim g As Graphics = Graphics.FromImage(canvas)
        Dim buff As New Bitmap(Mahjong.Width, Mahjong.Height)
        Mahjong.DrawToBitmap(buff, New Rectangle(0, 0, Mahjong.Width, Mahjong.Height))
        g.FillRectangle(Brushes.Black, New Rectangle(0, 0, canvas.Width, canvas.Height))
        For i = 0 To canvas.Height
            Dim x = (720 - i) * 0.15
            Dim x2 = 720 - x * 2
            Dim srcRect As New Rectangle(0, i, canvas.Width, 1)
            Dim desRect As New Rectangle(x, i, x2, 1)
            g.DrawImage(buff, desRect, srcRect, GraphicsUnit.Pixel)
        Next
        PictureBox1.Image = canvas
        PictureBox1.Visible = True
    End Sub
End Class