Public Class Form1
    Dim name_store(7, 11) As String
    Dim first_click As Boolean = False
    Dim name_store_temp As String
    Dim tag_store_temp As String
    Dim ar(7, 11) As Integer
    Dim address_array(1) As String
    Dim address_array0(1) As String
    Dim draw_coord_store(7) As Integer
    Dim draw_judge As Integer
    Dim counter As Integer
    Dim ii As Integer
    Dim jj As Integer
    Dim v_h_judge As Integer = 5
    Dim a_test As Integer
    Dim b_test As Integer
    Dim c_test As Integer
    Dim d_test As Integer
    Dim n_test As Integer
    Dim show_counter As Integer = 1
    Dim ran_temp As Integer
    Dim str_temp As String
    Dim combo As Integer
    Dim combo_time_counter As Integer

    'Dim path As String = Environment.CurrentDirectory

    Sub button_build()
        Dim obj As Button

        Dim bgNum As Integer

        Randomize()
        'bgNum = Int(Rnd() * 20) + 1
        bgNum = 6
        Me.BackgroundImage = System.Drawing.Image.FromFile("background\bb" & bgNum & ".png")

        For i = 0 To ar.GetUpperBound(0)
            For j = 0 To ar.GetUpperBound(1)
                obj = New Button
                obj.Name = Guid.NewGuid.ToString
                name_store(i, j) = obj.Name
                obj.Anchor = AnchorStyles.Left And AnchorStyles.Right And AnchorStyles.Bottom And AnchorStyles.Top
                obj.Left = Label2.Left + j * 50
                obj.Top = Label2.Top + i * 50
                obj.Text = ""
                obj.Width = 50
                obj.Height = 50
                obj.TabStop = False
                obj.Tag = i & "," & j & "," & ar(i, j)  'tag to store coordnation
                If ar(i, j) <> 0 Then
                    obj.ImageList = ImageList1
                    obj.ImageKey = ar(i, j) & ".png"

                    obj.FlatStyle = Windows.Forms.FlatStyle.Flat
                    obj.FlatAppearance.BorderSize = 0
                    obj.FlatAppearance.MouseDownBackColor = Color.Transparent
                    obj.FlatAppearance.MouseOverBackColor = Color.Transparent
                    obj.BackColor = Color.Transparent

                    AddHandler obj.Click, AddressOf Fd
                Else
                    obj.Visible = False      'the botton(invisible) is for later link lines convinence
                End If
                    Me.Controls.Add(obj)
            Next
        Next
        'generate a button matrix to realize the mouse-click operation
    End Sub

    Sub generate()
        'Dim countering_list(30) As Integer
        Dim countering_list(30) As Integer
        'use array countering_list() to limit the appearance time of a specific number
        Randomize()
        For i = 0 To ar.GetUpperBound(0)
            For j = 0 To ar.GetUpperBound(1)
                Do
                    If i = 0 Or j = 0 Or i = ar.GetUpperBound(0) Or j = ar.GetUpperBound(1) Then ar(i, j) = 0 : Exit Do
                    'ar(i, j) = Int(Rnd() * 30) + 1
                    ar(i, j) = Int(Rnd() * 30) + 1
                Loop While countering_list(ar(i, j)) >= 2
                'when the randomized number has appeared over 4 times, redo the assign job.
                countering_list(ar(i, j)) += 1
            Next
        Next
        'this part could be an subrountine to map editor, later
        Label1.Text = ""
        For i = 0 To ar.GetUpperBound(0)
            For j = 0 To ar.GetUpperBound(1)
                Label1.Text = Label1.Text & ar(i, j) & " "
            Next
            Label1.Text = Label1.Text & vbCrLf
        Next
        'generate a lebel to monitor the 2-dimension arrary ar()
        button_build()
    End Sub

    Sub Fd(ByVal sender As System.Object, ByVal e As System.EventArgs)
        
        'ReDim address_array(1)
        If first_click = False Then
            first_click = True
            name_store_temp = sender.name
            tag_store_temp = sender.tag
            Randomize()
            ran_temp = Int(Rnd() * 6)
            str_temp = "l" & Convert.ToString(ran_temp) & "."
            If InStr(sender.imagekey, "m") = 0 Then
                sender.imagekey = Replace(sender.imagekey, ".", str_temp)
            Else
                sender.imagekey = Replace(sender.imagekey, "m.", str_temp)
            End If
            'sender.imagekey = Replace(sender.imagekey, ".", str_temp)
            'If structure is for the "tip case", incase mess up the picture if user use tip.

        Else
            first_click = False
            ReDim address_array(2)
            address_array = sender.Tag.split(",") 'split tag
            Dim c() As Control = Me.Controls.Find(name_store_temp, False)
            ReDim address_array0(2)
            address_array0 = CType(c(0), Button).Tag.split(",")
            If (address_array(2) = address_array0(2)) And (sender.tag <> tag_store_temp) And (finallink(address_array(0), address_array(1), address_array0(0), address_array0(1)) = True) Then
                sender.visible = False
                ar(address_array(0), address_array(1)) = 0
                CType(c(0), Button).Visible = False
                ar(address_array0(0), address_array0(1)) = 0
                combo += 1
                combo_time_counter = 0
                combobox()
                'Debug.WriteLine(sender.tag)
                'Debug.WriteLine(sender.location)
                'Debug.WriteLine(CType(c(0), Button).Tag)
                'Debug.WriteLine(CType(c(0), Button).Location)
                Select Case draw_judge
                    Case 1
                        draw_line_one(address_array(0), address_array(1), address_array0(0), address_array0(1))
                    Case 2
                        draw_line_two(address_array(0), address_array(1), draw_coord_store(0), draw_coord_store(1), address_array0(0), address_array0(1))
                    Case 3
                        draw_line_three(address_array(0), address_array(1), draw_coord_store(2), draw_coord_store(3), draw_coord_store(4), draw_coord_store(5), address_array0(0), address_array0(1), draw_coord_store(6), draw_coord_store(7))
                End Select
            Else
                first_click = True
                CType(c(0), Button).ImageKey = Replace(CType(c(0), Button).ImageKey, str_temp, ".")
                name_store_temp = sender.name
                tag_store_temp = sender.tag
                Randomize()
                ran_temp = Int(Rnd() * 6)
                str_temp = "l" & Convert.ToString(ran_temp) & "."
                If InStr(sender.imagekey, "m") = 0 Then
                    sender.imagekey = Replace(sender.imagekey, ".", str_temp)
                Else
                    sender.imagekey = Replace(sender.imagekey, "m.", str_temp)
                End If
                'sender.imagekey = Replace(sender.imagekey, ".", str_temp)
                'If InStr(sender.imagekey, "m") = 0 Then sender.imagekey = Replace(sender.imagekey, ".", "m.")
            End If
        End If
        If success_test() = True Then
            MsgBox("OK")
            clean_button_memory()
            Exit Sub
        End If
        'judge_solution_possible_and_rearrange()
        Do While judge_solution_possible() = False
            rearrange()
        Loop
    End Sub

    Sub combobox()
        If combo = 0 Or combo = 1 Then
            PictureBox_combo.Visible = False
            PictureBox_combo1.Visible = False
            PictureBox_combo2.Visible = False

        ElseIf combo >= 2 And combo < 10 Then
            PictureBox_combo.Visible = True
            PictureBox_combo1.Visible = True
            PictureBox_combo1.ImageLocation = "combo\c" & combo & ".png"
        ElseIf combo >= 10 And combo <= 99 Then
            PictureBox_combo.Visible = True
            PictureBox_combo1.Visible = True
            PictureBox_combo1.ImageLocation = "combo\c" & Int(combo / 10) & ".png"
            PictureBox_combo2.Visible = True
            PictureBox_combo2.ImageLocation = "combo\c" & combo Mod 10 & ".png"
        Else
            PictureBox_combo.Visible = True
            PictureBox_combo1.Visible = True
            PictureBox_combo1.ImageLocation = "combo\c9.png"
            PictureBox_combo2.Visible = True
            PictureBox_combo2.ImageLocation = "combo\c9.png"
        End If
    End Sub
    Private Sub Timer1_Tick(sender As Object, e As EventArgs) Handles Timer1.Tick
        'Label1.Text = ""
        'For i = 0 To ar.GetUpperBound(0)
        '    For j = 0 To ar.GetUpperBound(1)
        '        Label1.Text = Label1.Text & ar(i, j) & " "
        '    Next
        '    Label1.Text = Label1.Text & vbCrLf
        'Next
        counter += 1
        If counter >= 4 Then
            Timer1.Stop()
            'Dim graphicsObject As Graphics
            'graphicsObject = Me.CreateGraphics()
            'graphicsObject.Clear(DefaultBackColor)
            clean_picbox()
        End If
       
    End Sub

    Function onelink(ByVal x1 As Integer, ByVal y1 As Integer, ByVal x0 As Integer, ByVal y0 As Integer) As Boolean
        If x1 = x0 Then
            If y1 < y0 Then
                For i = 1 To y0 - y1 - 1
                    If ar(x1, y1 + i) <> 0 Then Return False
                Next
                Return True
            Else
                For i = 1 To y1 - y0 - 1
                    If ar(x1, y0 + i) <> 0 Then Return False
                Next
                Return True
            End If
        End If
        If y1 = y0 Then
            If x1 < x0 Then
                For i = 1 To x0 - x1 - 1
                    If ar(x1 + i, y1) <> 0 Then Return False
                Next
                Return True
            Else
                For i = 1 To x1 - x0 - 1
                    If ar(x0 + i, y1) <> 0 Then Return False
                Next
                Return True
            End If
        End If
        Return False
    End Function

    Function twolink(ByVal x1 As Integer, ByVal y1 As Integer, ByVal x0 As Integer, ByVal y0 As Integer) As Boolean
        If (ar(x1, y0) = 0) And (onelink(x1, y0, x1, y1) = True) And (onelink(x1, y0, x0, y0) = True) Then
            draw_coord_store(0) = x1
            draw_coord_store(1) = y0
            Return True
        End If
        If (ar(x0, y1) = 0) And (onelink(x0, y1, x1, y1) = True) And (onelink(x0, y1, x0, y0) = True) Then
            draw_coord_store(0) = x0
            draw_coord_store(1) = y1
            Return True
        End If
        Return False
    End Function

    Function threelink(ByVal x1 As Integer, ByVal y1 As Integer, ByVal x0 As Integer, ByVal y0 As Integer) As Boolean
        Dim firstpoint_link_x() As Integer
        Dim firstpoint_link_y() As Integer
        Dim seconpoint_link_x() As Integer
        Dim seconpoint_link_y() As Integer
        Dim k As Integer = 0
        Dim l As Integer = 0
        Dim firstpoint_linku_x() As Integer
        Dim firstpoint_linku_y() As Integer
        Dim seconpoint_linku_x() As Integer
        Dim seconpoint_linku_y() As Integer
        Dim n As Integer = 0
        Dim m As Integer = 0
        For i = 0 To ar.GetUpperBound(0)
            For j = 0 To ar.GetUpperBound(1)
                If (ar(i, j) = 0) And (onelink(x1, y1, i, j) = True) Then
                    If i = x1 Then
                        ReDim Preserve firstpoint_link_x(k)
                        ReDim Preserve firstpoint_link_y(k)
                        firstpoint_link_x(k) = i
                        firstpoint_link_y(k) = j
                        k = k + 1
                    End If
                    If j = y1 Then
                        ReDim Preserve firstpoint_linku_x(n)
                        ReDim Preserve firstpoint_linku_y(n)
                        firstpoint_linku_x(n) = i
                        firstpoint_linku_y(n) = j
                        n = n + 1
                    End If
                End If
            Next
        Next
        For i = 0 To ar.GetUpperBound(0)
            For j = 0 To ar.GetUpperBound(1)
                If (ar(i, j) = 0) And (onelink(x0, y0, i, j) = True) Then
                    If i = x0 Then
                        ReDim Preserve seconpoint_link_x(l)
                        ReDim Preserve seconpoint_link_y(l)
                        seconpoint_link_x(l) = i
                        seconpoint_link_y(l) = j
                        l = l + 1
                    End If
                    If j = y0 Then
                        ReDim Preserve seconpoint_linku_x(m)
                        ReDim Preserve seconpoint_linku_y(m)
                        seconpoint_linku_x(m) = i
                        seconpoint_linku_y(m) = j
                        m = m + 1
                    End If
                End If
            Next
        Next
        If x1 <= x0 And y1 <= y0 Then
            For i = 0 To ar.GetUpperBound(0)
                For j = y1 To ar.GetUpperBound(1)
                    If (ar(i, j) = 0) Then
                        ReDim Preserve firstpoint_link_x(k - 1)
                        ReDim Preserve firstpoint_link_y(k - 1)
                        ReDim Preserve seconpoint_link_x(l - 1)
                        ReDim Preserve seconpoint_link_y(l - 1)
                        ii = i
                        jj = j
                        For k = 0 To firstpoint_link_x.GetUpperBound(0)
                            For l = 0 To seconpoint_link_x.GetUpperBound(0)
                                If ((onelink(i, j, firstpoint_link_x(k), firstpoint_link_y(k)) = True) Or ((i = firstpoint_link_x(k)) And (j = firstpoint_link_y(k)))) And ((onelink(i, j, seconpoint_link_x(l), seconpoint_link_y(l)) = True) Or ((i = seconpoint_link_x(l)) And (j = seconpoint_link_y(l)))) Then
                                    draw_coord_store(2) = firstpoint_link_x(k)
                                    draw_coord_store(3) = firstpoint_link_y(k)
                                    draw_coord_store(4) = seconpoint_link_x(l)
                                    draw_coord_store(5) = seconpoint_link_y(l)
                                    draw_coord_store(6) = ii
                                    draw_coord_store(7) = jj
                                    v_h_judge = 1
                                    Return True
                                End If
                            Next
                        Next
                    End If
                Next
            Next
            For i = 0 To ar.GetUpperBound(0)
                For j = y1 To 0 Step -1
                    If (ar(i, j) = 0) Then
                        ReDim Preserve firstpoint_link_x(k - 1)
                        ReDim Preserve firstpoint_link_y(k - 1)
                        ReDim Preserve seconpoint_link_x(l - 1)
                        ReDim Preserve seconpoint_link_y(l - 1)
                        ii = i
                        jj = j
                        For k = 0 To firstpoint_link_x.GetUpperBound(0)
                            For l = 0 To seconpoint_link_x.GetUpperBound(0)
                                If ((onelink(i, j, firstpoint_link_x(k), firstpoint_link_y(k)) = True) Or ((i = firstpoint_link_x(k)) And (j = firstpoint_link_y(k)))) And ((onelink(i, j, seconpoint_link_x(l), seconpoint_link_y(l)) = True) Or ((i = seconpoint_link_x(l)) And (j = seconpoint_link_y(l)))) Then
                                    draw_coord_store(2) = firstpoint_link_x(k)
                                    draw_coord_store(3) = firstpoint_link_y(k)
                                    draw_coord_store(4) = seconpoint_link_x(l)
                                    draw_coord_store(5) = seconpoint_link_y(l)
                                    draw_coord_store(6) = ii
                                    draw_coord_store(7) = jj
                                    v_h_judge = 1
                                    Return True
                                End If
                            Next
                        Next
                    End If
                Next
            Next
            For i = x1 To ar.GetUpperBound(0)
                For j = 0 To ar.GetUpperBound(1)
                    If (ar(i, j) = 0) Then
                        ReDim Preserve firstpoint_linku_x(n - 1)
                        ReDim Preserve firstpoint_linku_y(n - 1)
                        ReDim Preserve seconpoint_linku_x(m - 1)
                        ReDim Preserve seconpoint_linku_y(m - 1)
                        ii = i
                        jj = j
                        For n = 0 To firstpoint_linku_x.GetUpperBound(0)
                            For m = 0 To seconpoint_linku_x.GetUpperBound(0)
                                If ((onelink(i, j, firstpoint_linku_x(n), firstpoint_linku_y(n)) = True) Or ((i = firstpoint_linku_x(n)) And (j = firstpoint_linku_y(n)))) And ((onelink(i, j, seconpoint_linku_x(m), seconpoint_linku_y(m)) = True) Or ((i = seconpoint_linku_x(m)) And (j = seconpoint_linku_y(m)))) Then
                                    draw_coord_store(2) = firstpoint_linku_x(n)
                                    draw_coord_store(3) = firstpoint_linku_y(n)
                                    draw_coord_store(4) = seconpoint_linku_x(m)
                                    draw_coord_store(5) = seconpoint_linku_y(m)
                                    draw_coord_store(6) = ii
                                    draw_coord_store(7) = jj
                                    v_h_judge = 2
                                    Return True
                                End If
                            Next
                        Next
                    End If
                Next
            Next
            For i = x1 To 0 Step -1
                For j = 0 To ar.GetUpperBound(1)
                    If (ar(i, j) = 0) Then
                        ReDim Preserve firstpoint_linku_x(n - 1)
                        ReDim Preserve firstpoint_linku_y(n - 1)
                        ReDim Preserve seconpoint_linku_x(m - 1)
                        ReDim Preserve seconpoint_linku_y(m - 1)
                        ii = i
                        jj = j
                        For n = 0 To firstpoint_linku_x.GetUpperBound(0)
                            For m = 0 To seconpoint_linku_x.GetUpperBound(0)
                                If ((onelink(i, j, firstpoint_linku_x(n), firstpoint_linku_y(n)) = True) Or ((i = firstpoint_linku_x(n)) And (j = firstpoint_linku_y(n)))) And ((onelink(i, j, seconpoint_linku_x(m), seconpoint_linku_y(m)) = True) Or ((i = seconpoint_linku_x(m)) And (j = seconpoint_linku_y(m)))) Then
                                    draw_coord_store(2) = firstpoint_linku_x(n)
                                    draw_coord_store(3) = firstpoint_linku_y(n)
                                    draw_coord_store(4) = seconpoint_linku_x(m)
                                    draw_coord_store(5) = seconpoint_linku_y(m)
                                    draw_coord_store(6) = ii
                                    draw_coord_store(7) = jj
                                    v_h_judge = 2
                                    Return True
                                End If
                            Next
                        Next
                    End If
                Next
            Next
        End If
        If x1 <= x0 And y1 >= y0 Then
            For i = 0 To ar.GetUpperBound(0)
                For j = y1 To 0 Step -1       'zuo
                    If (ar(i, j) = 0) Then
                        ReDim Preserve firstpoint_link_x(k - 1)
                        ReDim Preserve firstpoint_link_y(k - 1)
                        ReDim Preserve seconpoint_link_x(l - 1)
                        ReDim Preserve seconpoint_link_y(l - 1)
                        ii = i
                        jj = j
                        For k = 0 To firstpoint_link_x.GetUpperBound(0)
                            For l = 0 To seconpoint_link_x.GetUpperBound(0)
                                If ((onelink(i, j, firstpoint_link_x(k), firstpoint_link_y(k)) = True) Or ((i = firstpoint_link_x(k)) And (j = firstpoint_link_y(k)))) And ((onelink(i, j, seconpoint_link_x(l), seconpoint_link_y(l)) = True) Or ((i = seconpoint_link_x(l)) And (j = seconpoint_link_y(l)))) Then
                                    draw_coord_store(2) = firstpoint_link_x(k)
                                    draw_coord_store(3) = firstpoint_link_y(k)
                                    draw_coord_store(4) = seconpoint_link_x(l)
                                    draw_coord_store(5) = seconpoint_link_y(l)
                                    draw_coord_store(6) = ii
                                    draw_coord_store(7) = jj
                                    v_h_judge = 1
                                    Return True
                                End If
                            Next
                        Next
                    End If
                Next
            Next
            For i = 0 To ar.GetUpperBound(0)   'you
                For j = y1 To ar.GetUpperBound(1)
                    If (ar(i, j) = 0) Then
                        ReDim Preserve firstpoint_link_x(k - 1)
                        ReDim Preserve firstpoint_link_y(k - 1)
                        ReDim Preserve seconpoint_link_x(l - 1)
                        ReDim Preserve seconpoint_link_y(l - 1)
                        ii = i
                        jj = j
                        For k = 0 To firstpoint_link_x.GetUpperBound(0)
                            For l = 0 To seconpoint_link_x.GetUpperBound(0)
                                If ((onelink(i, j, firstpoint_link_x(k), firstpoint_link_y(k)) = True) Or ((i = firstpoint_link_x(k)) And (j = firstpoint_link_y(k)))) And ((onelink(i, j, seconpoint_link_x(l), seconpoint_link_y(l)) = True) Or ((i = seconpoint_link_x(l)) And (j = seconpoint_link_y(l)))) Then
                                    draw_coord_store(2) = firstpoint_link_x(k)
                                    draw_coord_store(3) = firstpoint_link_y(k)
                                    draw_coord_store(4) = seconpoint_link_x(l)
                                    draw_coord_store(5) = seconpoint_link_y(l)
                                    draw_coord_store(6) = ii
                                    draw_coord_store(7) = jj
                                    v_h_judge = 1
                                    Return True
                                End If
                            Next
                        Next
                    End If
                Next
            Next
            For i = x1 To ar.GetUpperBound(0)
                For j = 0 To ar.GetUpperBound(1)        'xia
                    If (ar(i, j) = 0) Then
                        ReDim Preserve firstpoint_linku_x(n - 1)
                        ReDim Preserve firstpoint_linku_y(n - 1)
                        ReDim Preserve seconpoint_linku_x(m - 1)
                        ReDim Preserve seconpoint_linku_y(m - 1)
                        ii = i
                        jj = j
                        For n = 0 To firstpoint_linku_x.GetUpperBound(0)
                            For m = 0 To seconpoint_linku_x.GetUpperBound(0)
                                If ((onelink(i, j, firstpoint_linku_x(n), firstpoint_linku_y(n)) = True) Or ((i = firstpoint_linku_x(n)) And (j = firstpoint_linku_y(n)))) And ((onelink(i, j, seconpoint_linku_x(m), seconpoint_linku_y(m)) = True) Or ((i = seconpoint_linku_x(m)) And (j = seconpoint_linku_y(m)))) Then
                                    draw_coord_store(2) = firstpoint_linku_x(n)
                                    draw_coord_store(3) = firstpoint_linku_y(n)
                                    draw_coord_store(4) = seconpoint_linku_x(m)
                                    draw_coord_store(5) = seconpoint_linku_y(m)
                                    draw_coord_store(6) = ii
                                    draw_coord_store(7) = jj
                                    v_h_judge = 2
                                    Return True
                                End If
                            Next
                        Next
                    End If
                Next
            Next
            For i = x1 To 0 Step -1
                For j = 0 To ar.GetUpperBound(1)        'shang
                    If (ar(i, j) = 0) Then
                        ReDim Preserve firstpoint_linku_x(n - 1)
                        ReDim Preserve firstpoint_linku_y(n - 1)
                        ReDim Preserve seconpoint_linku_x(m - 1)
                        ReDim Preserve seconpoint_linku_y(m - 1)
                        ii = i
                        jj = j
                        For n = 0 To firstpoint_linku_x.GetUpperBound(0)
                            For m = 0 To seconpoint_linku_x.GetUpperBound(0)
                                If ((onelink(i, j, firstpoint_linku_x(n), firstpoint_linku_y(n)) = True) Or ((i = firstpoint_linku_x(n)) And (j = firstpoint_linku_y(n)))) And ((onelink(i, j, seconpoint_linku_x(m), seconpoint_linku_y(m)) = True) Or ((i = seconpoint_linku_x(m)) And (j = seconpoint_linku_y(m)))) Then
                                    draw_coord_store(2) = firstpoint_linku_x(n)
                                    draw_coord_store(3) = firstpoint_linku_y(n)
                                    draw_coord_store(4) = seconpoint_linku_x(m)
                                    draw_coord_store(5) = seconpoint_linku_y(m)
                                    draw_coord_store(6) = ii
                                    draw_coord_store(7) = jj
                                    v_h_judge = 2
                                    Return True
                                End If
                            Next
                        Next
                    End If
                Next
            Next
        End If
        If x1 >= x0 And y1 <= y0 Then
            For i = 0 To ar.GetUpperBound(0)   'you
                For j = y1 To ar.GetUpperBound(1)
                    If (ar(i, j) = 0) Then
                        ReDim Preserve firstpoint_link_x(k - 1)
                        ReDim Preserve firstpoint_link_y(k - 1)
                        ReDim Preserve seconpoint_link_x(l - 1)
                        ReDim Preserve seconpoint_link_y(l - 1)
                        ii = i
                        jj = j
                        For k = 0 To firstpoint_link_x.GetUpperBound(0)
                            For l = 0 To seconpoint_link_x.GetUpperBound(0)
                                If ((onelink(i, j, firstpoint_link_x(k), firstpoint_link_y(k)) = True) Or ((i = firstpoint_link_x(k)) And (j = firstpoint_link_y(k)))) And ((onelink(i, j, seconpoint_link_x(l), seconpoint_link_y(l)) = True) Or ((i = seconpoint_link_x(l)) And (j = seconpoint_link_y(l)))) Then
                                    draw_coord_store(2) = firstpoint_link_x(k)
                                    draw_coord_store(3) = firstpoint_link_y(k)
                                    draw_coord_store(4) = seconpoint_link_x(l)
                                    draw_coord_store(5) = seconpoint_link_y(l)
                                    draw_coord_store(6) = ii
                                    draw_coord_store(7) = jj
                                    v_h_judge = 1
                                    Return True
                                End If
                            Next
                        Next
                    End If
                Next
            Next
            For i = 0 To ar.GetUpperBound(0)
                For j = y1 To 0 Step -1       'zuo
                    If (ar(i, j) = 0) Then
                        ReDim Preserve firstpoint_link_x(k - 1)
                        ReDim Preserve firstpoint_link_y(k - 1)
                        ReDim Preserve seconpoint_link_x(l - 1)
                        ReDim Preserve seconpoint_link_y(l - 1)
                        ii = i
                        jj = j
                        For k = 0 To firstpoint_link_x.GetUpperBound(0)
                            For l = 0 To seconpoint_link_x.GetUpperBound(0)
                                If ((onelink(i, j, firstpoint_link_x(k), firstpoint_link_y(k)) = True) Or ((i = firstpoint_link_x(k)) And (j = firstpoint_link_y(k)))) And ((onelink(i, j, seconpoint_link_x(l), seconpoint_link_y(l)) = True) Or ((i = seconpoint_link_x(l)) And (j = seconpoint_link_y(l)))) Then
                                    draw_coord_store(2) = firstpoint_link_x(k)
                                    draw_coord_store(3) = firstpoint_link_y(k)
                                    draw_coord_store(4) = seconpoint_link_x(l)
                                    draw_coord_store(5) = seconpoint_link_y(l)
                                    draw_coord_store(6) = ii
                                    draw_coord_store(7) = jj
                                    v_h_judge = 1
                                    Return True
                                End If
                            Next
                        Next
                    End If
                Next
            Next
            For i = x1 To 0 Step -1
                For j = 0 To ar.GetUpperBound(1)        'shang
                    If (ar(i, j) = 0) Then
                        ReDim Preserve firstpoint_linku_x(n - 1)
                        ReDim Preserve firstpoint_linku_y(n - 1)
                        ReDim Preserve seconpoint_linku_x(m - 1)
                        ReDim Preserve seconpoint_linku_y(m - 1)
                        ii = i
                        jj = j
                        For n = 0 To firstpoint_linku_x.GetUpperBound(0)
                            For m = 0 To seconpoint_linku_x.GetUpperBound(0)
                                If ((onelink(i, j, firstpoint_linku_x(n), firstpoint_linku_y(n)) = True) Or ((i = firstpoint_linku_x(n)) And (j = firstpoint_linku_y(n)))) And ((onelink(i, j, seconpoint_linku_x(m), seconpoint_linku_y(m)) = True) Or ((i = seconpoint_linku_x(m)) And (j = seconpoint_linku_y(m)))) Then
                                    draw_coord_store(2) = firstpoint_linku_x(n)
                                    draw_coord_store(3) = firstpoint_linku_y(n)
                                    draw_coord_store(4) = seconpoint_linku_x(m)
                                    draw_coord_store(5) = seconpoint_linku_y(m)
                                    draw_coord_store(6) = ii
                                    draw_coord_store(7) = jj
                                    v_h_judge = 2
                                    Return True
                                End If
                            Next
                        Next
                    End If
                Next
            Next
            For i = x1 To ar.GetUpperBound(0)
                For j = 0 To ar.GetUpperBound(1)        'xia
                    If (ar(i, j) = 0) Then
                        ReDim Preserve firstpoint_linku_x(n - 1)
                        ReDim Preserve firstpoint_linku_y(n - 1)
                        ReDim Preserve seconpoint_linku_x(m - 1)
                        ReDim Preserve seconpoint_linku_y(m - 1)
                        ii = i
                        jj = j
                        For n = 0 To firstpoint_linku_x.GetUpperBound(0)
                            For m = 0 To seconpoint_linku_x.GetUpperBound(0)
                                If ((onelink(i, j, firstpoint_linku_x(n), firstpoint_linku_y(n)) = True) Or ((i = firstpoint_linku_x(n)) And (j = firstpoint_linku_y(n)))) And ((onelink(i, j, seconpoint_linku_x(m), seconpoint_linku_y(m)) = True) Or ((i = seconpoint_linku_x(m)) And (j = seconpoint_linku_y(m)))) Then
                                    draw_coord_store(2) = firstpoint_linku_x(n)
                                    draw_coord_store(3) = firstpoint_linku_y(n)
                                    draw_coord_store(4) = seconpoint_linku_x(m)
                                    draw_coord_store(5) = seconpoint_linku_y(m)
                                    draw_coord_store(6) = ii
                                    draw_coord_store(7) = jj
                                    v_h_judge = 2
                                    Return True
                                End If
                            Next
                        Next
                    End If
                Next
            Next
        End If
        If x1 >= x0 And y1 >= y0 Then
            For i = 0 To ar.GetUpperBound(0)
                For j = y1 To 0 Step -1       'zuo
                    If (ar(i, j) = 0) Then
                        ReDim Preserve firstpoint_link_x(k - 1)
                        ReDim Preserve firstpoint_link_y(k - 1)
                        ReDim Preserve seconpoint_link_x(l - 1)
                        ReDim Preserve seconpoint_link_y(l - 1)
                        ii = i
                        jj = j
                        For k = 0 To firstpoint_link_x.GetUpperBound(0)
                            For l = 0 To seconpoint_link_x.GetUpperBound(0)
                                If ((onelink(i, j, firstpoint_link_x(k), firstpoint_link_y(k)) = True) Or ((i = firstpoint_link_x(k)) And (j = firstpoint_link_y(k)))) And ((onelink(i, j, seconpoint_link_x(l), seconpoint_link_y(l)) = True) Or ((i = seconpoint_link_x(l)) And (j = seconpoint_link_y(l)))) Then
                                    draw_coord_store(2) = firstpoint_link_x(k)
                                    draw_coord_store(3) = firstpoint_link_y(k)
                                    draw_coord_store(4) = seconpoint_link_x(l)
                                    draw_coord_store(5) = seconpoint_link_y(l)
                                    draw_coord_store(6) = ii
                                    draw_coord_store(7) = jj
                                    v_h_judge = 1
                                    Return True
                                End If
                            Next
                        Next
                    End If
                Next
            Next
            For i = 0 To ar.GetUpperBound(0)   'you
                For j = y1 To ar.GetUpperBound(1)
                    If (ar(i, j) = 0) Then
                        ReDim Preserve firstpoint_link_x(k - 1)
                        ReDim Preserve firstpoint_link_y(k - 1)
                        ReDim Preserve seconpoint_link_x(l - 1)
                        ReDim Preserve seconpoint_link_y(l - 1)
                        ii = i
                        jj = j
                        For k = 0 To firstpoint_link_x.GetUpperBound(0)
                            For l = 0 To seconpoint_link_x.GetUpperBound(0)
                                If ((onelink(i, j, firstpoint_link_x(k), firstpoint_link_y(k)) = True) Or ((i = firstpoint_link_x(k)) And (j = firstpoint_link_y(k)))) And ((onelink(i, j, seconpoint_link_x(l), seconpoint_link_y(l)) = True) Or ((i = seconpoint_link_x(l)) And (j = seconpoint_link_y(l)))) Then
                                    draw_coord_store(2) = firstpoint_link_x(k)
                                    draw_coord_store(3) = firstpoint_link_y(k)
                                    draw_coord_store(4) = seconpoint_link_x(l)
                                    draw_coord_store(5) = seconpoint_link_y(l)
                                    draw_coord_store(6) = ii
                                    draw_coord_store(7) = jj
                                    v_h_judge = 1
                                    Return True
                                End If
                            Next
                        Next
                    End If
                Next
            Next
            For i = x1 To 0 Step -1
                For j = 0 To ar.GetUpperBound(1)        'shang
                    If (ar(i, j) = 0) Then
                        ReDim Preserve firstpoint_linku_x(n - 1)
                        ReDim Preserve firstpoint_linku_y(n - 1)
                        ReDim Preserve seconpoint_linku_x(m - 1)
                        ReDim Preserve seconpoint_linku_y(m - 1)
                        ii = i
                        jj = j
                        For n = 0 To firstpoint_linku_x.GetUpperBound(0)
                            For m = 0 To seconpoint_linku_x.GetUpperBound(0)
                                If ((onelink(i, j, firstpoint_linku_x(n), firstpoint_linku_y(n)) = True) Or ((i = firstpoint_linku_x(n)) And (j = firstpoint_linku_y(n)))) And ((onelink(i, j, seconpoint_linku_x(m), seconpoint_linku_y(m)) = True) Or ((i = seconpoint_linku_x(m)) And (j = seconpoint_linku_y(m)))) Then
                                    draw_coord_store(2) = firstpoint_linku_x(n)
                                    draw_coord_store(3) = firstpoint_linku_y(n)
                                    draw_coord_store(4) = seconpoint_linku_x(m)
                                    draw_coord_store(5) = seconpoint_linku_y(m)
                                    draw_coord_store(6) = ii
                                    draw_coord_store(7) = jj
                                    v_h_judge = 2
                                    Return True
                                End If
                            Next
                        Next
                    End If
                Next
            Next
            For i = x1 To ar.GetUpperBound(0)
                For j = 0 To ar.GetUpperBound(1)        'xia
                    If (ar(i, j) = 0) Then
                        ReDim Preserve firstpoint_linku_x(n - 1)
                        ReDim Preserve firstpoint_linku_y(n - 1)
                        ReDim Preserve seconpoint_linku_x(m - 1)
                        ReDim Preserve seconpoint_linku_y(m - 1)
                        ii = i
                        jj = j
                        For n = 0 To firstpoint_linku_x.GetUpperBound(0)
                            For m = 0 To seconpoint_linku_x.GetUpperBound(0)
                                If ((onelink(i, j, firstpoint_linku_x(n), firstpoint_linku_y(n)) = True) Or ((i = firstpoint_linku_x(n)) And (j = firstpoint_linku_y(n)))) And ((onelink(i, j, seconpoint_linku_x(m), seconpoint_linku_y(m)) = True) Or ((i = seconpoint_linku_x(m)) And (j = seconpoint_linku_y(m)))) Then
                                    draw_coord_store(2) = firstpoint_linku_x(n)
                                    draw_coord_store(3) = firstpoint_linku_y(n)
                                    draw_coord_store(4) = seconpoint_linku_x(m)
                                    draw_coord_store(5) = seconpoint_linku_y(m)
                                    draw_coord_store(6) = ii
                                    draw_coord_store(7) = jj
                                    v_h_judge = 2
                                    Return True
                                End If
                            Next
                        Next
                    End If
                Next
            Next
        End If
        Return False
    End Function

    Function finallink(ByVal x1 As Integer, ByVal y1 As Integer, ByVal x0 As Integer, ByVal y0 As Integer) As Boolean
        If onelink(x1, y1, x0, y0) = True Then
            draw_judge = 1
            Return True
        End If
        If twolink(x1, y1, x0, y0) = True Then
            draw_judge = 2
            Return True
        End If
        If threelink(x1, y1, x0, y0) = True Then
            draw_judge = 3
            Return True
        End If
        Return False
    End Function

    Function success_test() As Boolean
        For i = 0 To ar.GetUpperBound(0)
            For j = 0 To ar.GetUpperBound(1)
                If ar(i, j) <> 0 Then Return False
            Next
        Next
        Return True
    End Function




    Sub draw_line_one(x1 As Integer, y1 As Integer, x2 As Integer, y2 As Integer)
        Dim c() As Control = Me.Controls.Find(name_store(x1, y1), False)
        Dim d() As Control = Me.Controls.Find(name_store(x2, y2), False)
        clean_picbox()
        'Dim graphicsObject As Graphics
        'graphicsObject = Me.CreateGraphics()
        'graphicsObject.Clear(DefaultBackColor)
        'Dim pen_one As Pen
        'pen_one = New Pen(Color.Blue, 2)
        'graphicsObject.DrawLine(pen_one, CType(c(0), Button).Location.X + 25, CType(c(0), Button).Location.Y + 25, CType(d(0), Button).Location.X + 25, CType(d(0), Button).Location.Y + 25)
        Timer1.Start()
        counter = 0
        If (directing(x1, y1, x2, y2) = 1) Then
            PictureBox1.Visible = True
            PictureBox1.ImageLocation = "link\1.png"
            PictureBox1.Location = New Point(CType(c(0), Button).Location.X, CType(c(0), Button).Location.Y)
            PictureBox1.Width = CType(c(0), Button).Width
            PictureBox1.Height = CType(c(0), Button).Height * (x2 - x1 + 1)
        End If
        If (directing(x1, y1, x2, y2) = 2) Then
            PictureBox1.Visible = True
            PictureBox1.ImageLocation = "link\1.png"
            PictureBox1.Location = New Point(CType(d(0), Button).Location.X, CType(d(0), Button).Location.Y)
            PictureBox1.Width = CType(d(0), Button).Width
            PictureBox1.Height = CType(d(0), Button).Height * (x1 - x2 + 1)
        End If
        If (directing(x1, y1, x2, y2) = 3) Then
            PictureBox1.Visible = True
            PictureBox1.ImageLocation = "link\2.png"
            PictureBox1.Location = New Point(CType(d(0), Button).Location.X, CType(d(0), Button).Location.Y)
            PictureBox1.Width = CType(c(0), Button).Width * (y1 - y2 + 1)
            PictureBox1.Height = CType(c(0), Button).Height
        End If
        If (directing(x1, y1, x2, y2) = 4) Then
            PictureBox1.Visible = True
            PictureBox1.ImageLocation = "link\2.png"
            PictureBox1.Location = New Point(CType(c(0), Button).Location.X, CType(c(0), Button).Location.Y)
            PictureBox1.Width = CType(c(0), Button).Width * (y2 - y1 + 1)
            PictureBox1.Height = CType(c(0), Button).Height
        End If
    End Sub

    Sub draw_line_two(x1 As Integer, y1 As Integer, x2 As Integer, y2 As Integer, x3 As Integer, y3 As Integer)
        Dim c() As Control = Me.Controls.Find(name_store(x1, y1), False)
        Dim d() As Control = Me.Controls.Find(name_store(x2, y2), False)
        Dim e() As Control = Me.Controls.Find(name_store(x3, y3), False)
        clean_picbox()
        'Dim graphicsObject As Graphics
        'graphicsObject = Me.CreateGraphics()
        'graphicsObject.Clear(DefaultBackColor)
        'Dim pen_one As Pen
        'pen_one = New Pen(Color.Blue, 2)
        'graphicsObject.DrawLine(pen_one, CType(c(0), Button).Location.X + 25, CType(c(0), Button).Location.Y + 25, CType(d(0), Button).Location.X + 25, CType(d(0), Button).Location.Y + 25)
        If (directing(x1, y1, x2, y2) = 1) Then
            PictureBox1.Visible = True
            PictureBox1.ImageLocation = "link\3.png"
            PictureBox1.Location = New Point(CType(c(0), Button).Location.X, CType(c(0), Button).Location.Y)
            PictureBox1.Width = CType(c(0), Button).Width
            PictureBox1.Height = CType(c(0), Button).Height * (x2 - x1)
        End If
        If (directing(x1, y1, x2, y2) = 2) Then
            PictureBox1.Visible = True
            PictureBox1.ImageLocation = "link\4.png"
            PictureBox1.Location = New Point(CType(d(0), Button).Location.X, CType(d(0), Button).Location.Y + CType(d(0), Button).Height)
            PictureBox1.Width = CType(d(0), Button).Width
            PictureBox1.Height = CType(d(0), Button).Height * (x1 - x2)
        End If
        If (directing(x1, y1, x2, y2) = 3) Then
            PictureBox1.Visible = True
            PictureBox1.ImageLocation = "link\5.png"
            PictureBox1.Location = New Point(CType(d(0), Button).Location.X + CType(d(0), Button).Width, CType(d(0), Button).Location.Y)
            PictureBox1.Width = CType(c(0), Button).Width * (y1 - y2)
            PictureBox1.Height = CType(c(0), Button).Height
        End If
        If (directing(x1, y1, x2, y2) = 4) Then
            PictureBox1.Visible = True
            PictureBox1.ImageLocation = "link\6.png"
            PictureBox1.Location = New Point(CType(c(0), Button).Location.X, CType(c(0), Button).Location.Y)
            PictureBox1.Width = CType(c(0), Button).Width * (y2 - y1)
            PictureBox1.Height = CType(c(0), Button).Height
        End If
        If (directing(x2, y2, x1, y1) = 1 And (directing(x2, y2, x3, y3) = 3)) Or ((directing(x2, y2, x1, y1) = 3 And (directing(x2, y2, x3, y3) = 1))) Then
            PictureBox2.Visible = True
            PictureBox2.ImageLocation = "link\7.png"
            PictureBox2.Location = New Point(CType(d(0), Button).Location.X, CType(d(0), Button).Location.Y)
        End If
        If (directing(x2, y2, x1, y1) = 1 And (directing(x2, y2, x3, y3) = 4)) Or ((directing(x2, y2, x1, y1) = 4 And (directing(x2, y2, x3, y3) = 1))) Then
            PictureBox2.Visible = True
            PictureBox2.ImageLocation = "link\8.png"
            PictureBox2.Location = New Point(CType(d(0), Button).Location.X, CType(d(0), Button).Location.Y)
        End If
        If ((directing(x2, y2, x1, y1) = 2 And (directing(x2, y2, x3, y3) = 3))) Or ((directing(x2, y2, x1, y1) = 3 And (directing(x2, y2, x3, y3) = 2))) Then
            PictureBox2.Visible = True
            PictureBox2.ImageLocation = "link\9.png"
            PictureBox2.Location = New Point(CType(d(0), Button).Location.X, CType(d(0), Button).Location.Y)
        End If
        If (directing(x2, y2, x1, y1) = 2 And (directing(x2, y2, x3, y3) = 4)) Or ((directing(x2, y2, x1, y1) = 4 And (directing(x2, y2, x3, y3) = 2))) Then
            PictureBox2.Visible = True
            PictureBox2.ImageLocation = "link\10.png"
            PictureBox2.Location = New Point(CType(d(0), Button).Location.X, CType(d(0), Button).Location.Y)
        End If
        'graphicsObject.DrawLine(pen_one, CType(d(0), Button).Location.X + 25, CType(d(0), Button).Location.Y + 25, CType(e(0), Button).Location.X + 25, CType(e(0), Button).Location.Y + 25)
        If directing(x3, y3, x2, y2) = 2 Then
            PictureBox3.Visible = True
            PictureBox3.ImageLocation = "link\4.png"
            PictureBox3.Location = New Point(CType(d(0), Button).Location.X, CType(d(0), Button).Location.Y + CType(d(0), Button).Height)
            PictureBox3.Width = CType(e(0), Button).Width
            PictureBox3.Height = CType(e(0), Button).Height * (x3 - x2)
        End If
        If directing(x3, y3, x2, y2) = 1 Then
            PictureBox3.Visible = True
            PictureBox3.ImageLocation = "link\3.png"
            PictureBox3.Location = New Point(CType(e(0), Button).Location.X, CType(e(0), Button).Location.Y)
            PictureBox3.Width = CType(e(0), Button).Width
            PictureBox3.Height = CType(e(0), Button).Height * (x2 - x3)
        End If
        If directing(x3, y3, x2, y2) = 3 Then
            PictureBox3.Visible = True
            PictureBox3.ImageLocation = "link\5.png"
            PictureBox3.Location = New Point(CType(d(0), Button).Location.X + CType(d(0), Button).Width, CType(d(0), Button).Location.Y)
            PictureBox3.Width = CType(e(0), Button).Width * (y3 - y2)
            PictureBox3.Height = CType(e(0), Button).Height
        End If
        If directing(x3, y3, x2, y2) = 4 Then
            PictureBox3.Visible = True
            PictureBox3.ImageLocation = "link\6.png"
            PictureBox3.Location = New Point(CType(e(0), Button).Location.X, CType(e(0), Button).Location.Y)
            PictureBox3.Width = CType(e(0), Button).Width * (y2 - y3)
            PictureBox3.Height = CType(e(0), Button).Height
        End If
        Timer1.Start()
        counter = 0

    End Sub

    Sub clean_picbox()
        PictureBox1.Visible = False
        PictureBox2.Visible = False
        PictureBox3.Visible = False
        PictureBox4.Visible = False
        PictureBox5.Visible = False
    End Sub

    Sub draw_line_three(x1 As Integer, y1 As Integer, x2 As Integer, y2 As Integer, x3 As Integer, y3 As Integer, x4 As Integer, y4 As Integer, x5 As Integer, y5 As Integer)
        Dim c() As Control = Me.Controls.Find(name_store(x1, y1), False)
        Dim d() As Control = Me.Controls.Find(name_store(x2, y2), False)
        Dim e() As Control = Me.Controls.Find(name_store(x3, y3), False)
        Dim f() As Control = Me.Controls.Find(name_store(x4, y4), False)
        Dim g() As Control = Me.Controls.Find(name_store(x5, y5), False)
        clean_picbox()
        'Dim graphicsObject As Graphics
        'graphicsObject = Me.CreateGraphics()
        'graphicsObject.Clear(DefaultBackColor)
        'Dim pen_one As Pen
        'pen_one = New Pen(Color.Blue, 2)
        If x2 <> x3 And y2 <> y3 Then
            If v_h_judge = 1 Then
                If x2 = x5 Then
                    'graphicsObject.DrawLine(pen_one, CType(c(0), Button).Location.X + 25, CType(c(0), Button).Location.Y + 25, CType(g(0), Button).Location.X + 25, CType(g(0), Button).Location.Y + 25)
                    'graphicsObject.DrawLine(pen_one, CType(g(0), Button).Location.X + 25, CType(g(0), Button).Location.Y + 25, CType(e(0), Button).Location.X + 25, CType(e(0), Button).Location.Y + 25)
                    'graphicsObject.DrawLine(pen_one, CType(e(0), Button).Location.X + 25, CType(e(0), Button).Location.Y + 25, CType(f(0), Button).Location.X + 25, CType(f(0), Button).Location.Y + 25)
                    If (directing(x1, y1, x5, y5) = 1) Then
                        PictureBox1.Visible = True
                        PictureBox1.ImageLocation = "link\3.png"
                        PictureBox1.Location = New Point(CType(c(0), Button).Location.X, CType(c(0), Button).Location.Y)
                        PictureBox1.Width = CType(c(0), Button).Width
                        PictureBox1.Height = CType(c(0), Button).Height * (x5 - x1)
                    End If
                    If (directing(x1, y1, x5, y5) = 2) Then
                        PictureBox1.Visible = True
                        PictureBox1.ImageLocation = "link\4.png"
                        PictureBox1.Location = New Point(CType(g(0), Button).Location.X, CType(g(0), Button).Location.Y + CType(g(0), Button).Height)
                        PictureBox1.Width = CType(g(0), Button).Width
                        PictureBox1.Height = CType(g(0), Button).Height * (x1 - x5)
                    End If
                    If (directing(x1, y1, x5, y5) = 3) Then
                        PictureBox1.Visible = True
                        PictureBox1.ImageLocation = "link\5.png"
                        PictureBox1.Location = New Point(CType(g(0), Button).Location.X + CType(g(0), Button).Width, CType(g(0), Button).Location.Y)
                        PictureBox1.Width = CType(g(0), Button).Width * (y1 - y5)
                        PictureBox1.Height = CType(g(0), Button).Height
                    End If
                    If (directing(x1, y1, x5, y5) = 4) Then
                        PictureBox1.Visible = True
                        PictureBox1.ImageLocation = "link\6.png"
                        PictureBox1.Location = New Point(CType(c(0), Button).Location.X, CType(c(0), Button).Location.Y)
                        PictureBox1.Width = CType(c(0), Button).Width * (y5 - y1)
                        PictureBox1.Height = CType(c(0), Button).Height
                    End If
                    If (directing(x5, y5, x1, y1) = 1 And (directing(x5, y5, x3, y3) = 3)) Or ((directing(x5, y5, x1, y1) = 3 And (directing(x5, y5, x3, y3) = 1))) Then
                        PictureBox2.Visible = True
                        PictureBox2.ImageLocation = "link\7.png"
                        PictureBox2.Location = New Point(CType(g(0), Button).Location.X, CType(g(0), Button).Location.Y)
                    End If
                    If (directing(x5, y5, x1, y1) = 1 And (directing(x5, y5, x3, y3) = 4)) Or ((directing(x5, y5, x1, y1) = 4 And (directing(x5, y5, x3, y3) = 1))) Then
                        PictureBox2.Visible = True
                        PictureBox2.ImageLocation = "link\8.png"
                        PictureBox2.Location = New Point(CType(g(0), Button).Location.X, CType(g(0), Button).Location.Y)
                    End If
                    If ((directing(x5, y5, x1, y1) = 2 And (directing(x5, y5, x3, y3) = 3))) Or ((directing(x5, y5, x1, y1) = 3 And (directing(x5, y5, x3, y3) = 2))) Then
                        PictureBox2.Visible = True
                        PictureBox2.ImageLocation = "link\9.png"
                        PictureBox2.Location = New Point(CType(g(0), Button).Location.X, CType(g(0), Button).Location.Y)
                    End If
                    If (directing(x5, y5, x1, y1) = 2 And (directing(x5, y5, x3, y3) = 4)) Or ((directing(x5, y5, x1, y1) = 4 And (directing(x5, y5, x3, y3) = 2))) Then
                        PictureBox2.Visible = True
                        PictureBox2.ImageLocation = "link\10.png"
                        PictureBox2.Location = New Point(CType(g(0), Button).Location.X, CType(g(0), Button).Location.Y)
                    End If
                    If directing(x5, y5, x3, y3) = 2 Then
                        PictureBox3.Visible = True
                        PictureBox3.ImageLocation = "link\11.png"
                        PictureBox3.Location = New Point(CType(e(0), Button).Location.X, CType(e(0), Button).Location.Y + CType(e(0), Button).Height)
                        PictureBox3.Width = CType(e(0), Button).Width
                        PictureBox3.Height = CType(e(0), Button).Height * (x5 - x3 - 1)
                    End If
                    If directing(x5, y5, x3, y3) = 1 Then
                        PictureBox3.Visible = True
                        PictureBox3.ImageLocation = "link\11.png"
                        PictureBox3.Location = New Point(CType(g(0), Button).Location.X, CType(g(0), Button).Location.Y + CType(g(0), Button).Height)
                        PictureBox3.Width = CType(g(0), Button).Width
                        PictureBox3.Height = CType(g(0), Button).Height * (x3 - x5 - 1)
                    End If
                    If directing(x5, y5, x3, y3) = 3 Then
                        PictureBox3.Visible = True
                        PictureBox3.ImageLocation = "link\12.png"
                        PictureBox3.Location = New Point(CType(e(0), Button).Location.X + CType(e(0), Button).Width, CType(e(0), Button).Location.Y)
                        PictureBox3.Width = CType(e(0), Button).Width * (y5 - y3 - 1)
                        PictureBox3.Height = CType(e(0), Button).Height
                    End If
                    If directing(x5, y5, x3, y3) = 4 Then
                        PictureBox3.Visible = True
                        PictureBox3.ImageLocation = "link\12.png"
                        PictureBox3.Location = New Point(CType(g(0), Button).Location.X + CType(g(0), Button).Width, CType(g(0), Button).Location.Y)
                        PictureBox3.Width = CType(g(0), Button).Width * (y3 - y5 - 1)
                        PictureBox3.Height = CType(g(0), Button).Height
                    End If
                    If ((directing(x3, y3, x5, y5) = 2 And (directing(x3, y3, x4, y4) = 3))) Or ((directing(x3, y3, x5, y5) = 3 And (directing(x3, y3, x4, y4) = 2))) Then
                        PictureBox4.Visible = True
                        PictureBox4.ImageLocation = "link\9.png"
                        PictureBox4.Location = New Point(CType(e(0), Button).Location.X, CType(e(0), Button).Location.Y)
                    End If
                    If ((directing(x3, y3, x5, y5) = 2 And (directing(x3, y3, x4, y4) = 4))) Or ((directing(x3, y3, x5, y5) = 4 And (directing(x3, y3, x4, y4) = 2))) Then
                        PictureBox4.Visible = True
                        PictureBox4.ImageLocation = "link\10.png"
                        PictureBox4.Location = New Point(CType(e(0), Button).Location.X, CType(e(0), Button).Location.Y)
                    End If
                    If ((directing(x3, y3, x5, y5) = 1 And (directing(x3, y3, x4, y4) = 3))) Or ((directing(x3, y3, x5, y5) = 3 And (directing(x3, y3, x4, y4) = 1))) Then
                        PictureBox4.Visible = True
                        PictureBox4.ImageLocation = "link\7.png"
                        PictureBox4.Location = New Point(CType(e(0), Button).Location.X, CType(e(0), Button).Location.Y)
                    End If
                    If ((directing(x3, y3, x5, y5) = 1 And (directing(x3, y3, x4, y4) = 4))) Or ((directing(x3, y3, x5, y5) = 4 And (directing(x3, y3, x4, y4) = 1))) Then
                        PictureBox4.Visible = True
                        PictureBox4.ImageLocation = "link\8.png"
                        PictureBox4.Location = New Point(CType(e(0), Button).Location.X, CType(e(0), Button).Location.Y)
                    End If
                    If directing(x4, y4, x3, y3) = 2 Then
                        PictureBox5.Visible = True
                        PictureBox5.ImageLocation = "link\4.png"
                        PictureBox5.Location = New Point(CType(e(0), Button).Location.X, CType(e(0), Button).Location.Y + CType(e(0), Button).Height)
                        PictureBox5.Width = CType(e(0), Button).Width
                        PictureBox5.Height = CType(e(0), Button).Height * (x4 - x3)
                    End If
                    If directing(x4, y4, x3, y3) = 1 Then
                        PictureBox5.Visible = True
                        PictureBox5.ImageLocation = "link\3.png"
                        PictureBox5.Location = New Point(CType(f(0), Button).Location.X, CType(f(0), Button).Location.Y)
                        PictureBox5.Width = CType(f(0), Button).Width
                        PictureBox5.Height = CType(f(0), Button).Height * (x3 - x4)
                    End If
                    If directing(x4, y4, x3, y3) = 3 Then
                        PictureBox5.Visible = True
                        PictureBox5.ImageLocation = "link\5.png"
                        PictureBox5.Location = New Point(CType(e(0), Button).Location.X + CType(e(0), Button).Height, CType(e(0), Button).Location.Y)
                        PictureBox5.Width = CType(e(0), Button).Width * (y4 - y3)
                        PictureBox5.Height = CType(e(0), Button).Height
                    End If
                    If directing(x4, y4, x3, y3) = 4 Then
                        PictureBox5.Visible = True
                        PictureBox5.ImageLocation = "link\6.png"
                        PictureBox5.Location = New Point(CType(f(0), Button).Location.X, CType(f(0), Button).Location.Y)
                        PictureBox5.Width = CType(f(0), Button).Width * (y3 - y4)
                        PictureBox5.Height = CType(f(0), Button).Height
                    End If
                    counter = 0
                    Timer1.Start()
                    Debug.WriteLine("A" & x1 & "," & y1 & "," & x2 & "," & y2 & "," & x3 & "," & y3 & "," & x4 & "," & y4 & "," & x5 & "," & y5)
                    a_test += 1
                Else
                    'graphicsObject.DrawLine(pen_one, CType(c(0), Button).Location.X + 25, CType(c(0), Button).Location.Y + 25, CType(d(0), Button).Location.X + 25, CType(d(0), Button).Location.Y + 25)
                    'graphicsObject.DrawLine(pen_one, CType(d(0), Button).Location.X + 25, CType(d(0), Button).Location.Y + 25, CType(g(0), Button).Location.X + 25, CType(g(0), Button).Location.Y + 25)
                    'graphicsObject.DrawLine(pen_one, CType(g(0), Button).Location.X + 25, CType(g(0), Button).Location.Y + 25, CType(f(0), Button).Location.X + 25, CType(f(0), Button).Location.Y + 25)
                    If (directing(x1, y1, x2, y2) = 1) Then
                        PictureBox1.Visible = True
                        PictureBox1.ImageLocation = "link\3.png"
                        PictureBox1.Location = New Point(CType(c(0), Button).Location.X, CType(c(0), Button).Location.Y)
                        PictureBox1.Width = CType(c(0), Button).Width
                        PictureBox1.Height = CType(c(0), Button).Height * (x2 - x1)
                    End If
                    If (directing(x1, y1, x2, y2) = 2) Then
                        PictureBox1.Visible = True
                        PictureBox1.ImageLocation = "link\4.png"
                        PictureBox1.Location = New Point(CType(d(0), Button).Location.X, CType(d(0), Button).Location.Y + CType(d(0), Button).Height)
                        PictureBox1.Width = CType(d(0), Button).Width
                        PictureBox1.Height = CType(d(0), Button).Height * (x1 - x2)
                    End If
                    If (directing(x1, y1, x2, y2) = 3) Then
                        PictureBox1.Visible = True
                        PictureBox1.ImageLocation = "link\5.png"
                        PictureBox1.Location = New Point(CType(d(0), Button).Location.X + CType(d(0), Button).Width, CType(d(0), Button).Location.Y)
                        PictureBox1.Width = CType(d(0), Button).Width * (y1 - y2)
                        PictureBox1.Height = CType(d(0), Button).Height
                    End If
                    If (directing(x1, y1, x2, y2) = 4) Then
                        PictureBox1.Visible = True
                        PictureBox1.ImageLocation = "link\6.png"
                        PictureBox1.Location = New Point(CType(c(0), Button).Location.X, CType(c(0), Button).Location.Y)
                        PictureBox1.Width = CType(c(0), Button).Width * (y2 - y1)
                        PictureBox1.Height = CType(c(0), Button).Height
                    End If
                    If (directing(x2, y2, x1, y1) = 1 And (directing(x2, y2, x5, y5) = 3)) Or ((directing(x2, y2, x1, y1) = 3 And (directing(x2, y2, x5, y5) = 1))) Then
                        PictureBox2.Visible = True
                        PictureBox2.ImageLocation = "link\7.png"
                        PictureBox2.Location = New Point(CType(d(0), Button).Location.X, CType(d(0), Button).Location.Y)
                    End If
                    If (directing(x2, y2, x1, y1) = 1 And (directing(x2, y2, x5, y5) = 4)) Or ((directing(x2, y2, x1, y1) = 4 And (directing(x2, y2, x5, y5) = 1))) Then
                        PictureBox2.Visible = True
                        PictureBox2.ImageLocation = "link\8.png"
                        PictureBox2.Location = New Point(CType(d(0), Button).Location.X, CType(d(0), Button).Location.Y)
                    End If
                    If ((directing(x2, y2, x1, y1) = 2 And (directing(x2, y2, x5, y5) = 3))) Or ((directing(x2, y2, x1, y1) = 3 And (directing(x2, y2, x5, y5) = 2))) Then
                        PictureBox2.Visible = True
                        PictureBox2.ImageLocation = "link\9.png"
                        PictureBox2.Location = New Point(CType(d(0), Button).Location.X, CType(d(0), Button).Location.Y)
                    End If
                    If (directing(x2, y2, x1, y1) = 2 And (directing(x2, y2, x5, y5) = 4)) Or ((directing(x2, y2, x1, y1) = 4 And (directing(x2, y2, x5, y5) = 2))) Then
                        PictureBox2.Visible = True
                        PictureBox2.ImageLocation = "link\10.png"
                        PictureBox2.Location = New Point(CType(d(0), Button).Location.X, CType(d(0), Button).Location.Y)
                    End If
                    If directing(x2, y2, x5, y5) = 2 Then
                        PictureBox3.Visible = True
                        PictureBox3.ImageLocation = "link\11.png"
                        PictureBox3.Location = New Point(CType(g(0), Button).Location.X, CType(g(0), Button).Location.Y + CType(g(0), Button).Height)
                        PictureBox3.Width = CType(g(0), Button).Width
                        PictureBox3.Height = CType(g(0), Button).Height * (x2 - x5 - 1)
                    End If
                    If directing(x2, y2, x5, y5) = 1 Then
                        PictureBox3.Visible = True
                        PictureBox3.ImageLocation = "link\11.png"
                        PictureBox3.Location = New Point(CType(d(0), Button).Location.X, CType(d(0), Button).Location.Y + CType(d(0), Button).Height)
                        PictureBox3.Width = CType(d(0), Button).Width
                        PictureBox3.Height = CType(d(0), Button).Height * (x5 - x2 - 1)
                    End If
                    If directing(x2, y2, x5, y5) = 3 Then
                        PictureBox3.Visible = True
                        PictureBox3.ImageLocation = "link\12.png"
                        PictureBox3.Location = New Point(CType(g(0), Button).Location.X + CType(g(0), Button).Width, CType(g(0), Button).Location.Y)
                        PictureBox3.Width = CType(g(0), Button).Width * (y2 - y5 - 1)
                        PictureBox3.Height = CType(g(0), Button).Height
                    End If
                    If directing(x2, y2, x5, y5) = 4 Then
                        PictureBox3.Visible = True
                        PictureBox3.ImageLocation = "link\12.png"
                        PictureBox3.Location = New Point(CType(d(0), Button).Location.X + CType(d(0), Button).Width, CType(d(0), Button).Location.Y)
                        PictureBox3.Width = CType(d(0), Button).Width * (y5 - y2 - 1)
                        PictureBox3.Height = CType(d(0), Button).Height
                    End If
                    If ((directing(x5, y5, x2, y2) = 2 And (directing(x5, y5, x4, y4) = 3))) Or ((directing(x5, y5, x2, y2) = 3 And (directing(x5, y5, x4, y4) = 2))) Then
                        PictureBox4.Visible = True
                        PictureBox4.ImageLocation = "link\9.png"
                        PictureBox4.Location = New Point(CType(g(0), Button).Location.X, CType(g(0), Button).Location.Y)
                    End If
                    If ((directing(x5, y5, x2, y2) = 2 And (directing(x5, y5, x4, y4) = 4))) Or ((directing(x5, y5, x2, y2) = 4 And (directing(x5, y5, x4, y4) = 2))) Then
                        PictureBox4.Visible = True
                        PictureBox4.ImageLocation = "link\10.png"
                        PictureBox4.Location = New Point(CType(g(0), Button).Location.X, CType(g(0), Button).Location.Y)
                    End If
                    If ((directing(x5, y5, x2, y2) = 1 And (directing(x5, y5, x4, y4) = 3))) Or ((directing(x5, y5, x2, y2) = 3 And (directing(x5, y5, x4, y4) = 1))) Then
                        PictureBox4.Visible = True
                        PictureBox4.ImageLocation = "link\7.png"
                        PictureBox4.Location = New Point(CType(g(0), Button).Location.X, CType(g(0), Button).Location.Y)
                    End If
                    If ((directing(x5, y5, x2, y2) = 1 And (directing(x5, y5, x4, y4) = 4))) Or ((directing(x5, y5, x2, y2) = 4 And (directing(x5, y5, x4, y4) = 1))) Then
                        PictureBox4.Visible = True
                        PictureBox4.ImageLocation = "link\8.png"
                        PictureBox4.Location = New Point(CType(g(0), Button).Location.X, CType(g(0), Button).Location.Y)
                    End If
                    If directing(x4, y4, x5, y5) = 2 Then
                        PictureBox5.Visible = True
                        PictureBox5.ImageLocation = "link\4.png"
                        PictureBox5.Location = New Point(CType(g(0), Button).Location.X, CType(g(0), Button).Location.Y + CType(g(0), Button).Height)
                        PictureBox5.Width = CType(g(0), Button).Width
                        PictureBox5.Height = CType(g(0), Button).Height * (x4 - x5)
                    End If
                    If directing(x4, y4, x5, y5) = 1 Then
                        PictureBox5.Visible = True
                        PictureBox5.ImageLocation = "link\3.png"
                        PictureBox5.Location = New Point(CType(f(0), Button).Location.X, CType(f(0), Button).Location.Y)
                        PictureBox5.Width = CType(f(0), Button).Width
                        PictureBox5.Height = CType(f(0), Button).Height * (x5 - x4)
                    End If
                    If directing(x4, y4, x5, y5) = 3 Then
                        PictureBox5.Visible = True
                        PictureBox5.ImageLocation = "link\5.png"
                        PictureBox5.Location = New Point(CType(g(0), Button).Location.X + CType(g(0), Button).Height, CType(g(0), Button).Location.Y)
                        PictureBox5.Width = CType(g(0), Button).Width * (y4 - y5)
                        PictureBox5.Height = CType(g(0), Button).Height
                    End If
                    If directing(x4, y4, x5, y5) = 4 Then
                        PictureBox5.Visible = True
                        PictureBox5.ImageLocation = "link\6.png"
                        PictureBox5.Location = New Point(CType(f(0), Button).Location.X, CType(f(0), Button).Location.Y)
                        PictureBox5.Width = CType(f(0), Button).Width * (y5 - y4)
                        PictureBox5.Height = CType(f(0), Button).Height
                    End If
                    counter = 0
                    Timer1.Start()
                    Debug.WriteLine("B" & x1 & "," & y1 & "," & x2 & "," & y2 & "," & x3 & "," & y3 & "," & x4 & "," & y4 & "," & x5 & "," & y5)
                    b_test += 1
                End If
            Else
                If y2 = y5 Then
                    'graphicsObject.DrawLine(pen_one, CType(c(0), Button).Location.X + 25, CType(c(0), Button).Location.Y + 25, CType(g(0), Button).Location.X + 25, CType(g(0), Button).Location.Y + 25)
                    'graphicsObject.DrawLine(pen_one, CType(g(0), Button).Location.X + 25, CType(g(0), Button).Location.Y + 25, CType(e(0), Button).Location.X + 25, CType(e(0), Button).Location.Y + 25)
                    'graphicsObject.DrawLine(pen_one, CType(e(0), Button).Location.X + 25, CType(e(0), Button).Location.Y + 25, CType(f(0), Button).Location.X + 25, CType(f(0), Button).Location.Y + 25)
                    If (directing(x1, y1, x5, y5) = 1) Then
                        PictureBox1.Visible = True
                        PictureBox1.ImageLocation = "link\3.png"
                        PictureBox1.Location = New Point(CType(c(0), Button).Location.X, CType(c(0), Button).Location.Y)
                        PictureBox1.Width = CType(c(0), Button).Width
                        PictureBox1.Height = CType(c(0), Button).Height * (x5 - x1)
                    End If
                    If (directing(x1, y1, x5, y5) = 2) Then
                        PictureBox1.Visible = True
                        PictureBox1.ImageLocation = "link\4.png"
                        PictureBox1.Location = New Point(CType(g(0), Button).Location.X, CType(g(0), Button).Location.Y + CType(g(0), Button).Height)
                        PictureBox1.Width = CType(g(0), Button).Width
                        PictureBox1.Height = CType(g(0), Button).Height * (x1 - x5)
                    End If
                    If (directing(x1, y1, x5, y5) = 3) Then
                        PictureBox1.Visible = True
                        PictureBox1.ImageLocation = "link\5.png"
                        PictureBox1.Location = New Point(CType(g(0), Button).Location.X + CType(g(0), Button).Width, CType(g(0), Button).Location.Y)
                        PictureBox1.Width = CType(g(0), Button).Width * (y1 - y5)
                        PictureBox1.Height = CType(g(0), Button).Height
                    End If
                    If (directing(x1, y1, x5, y5) = 4) Then
                        PictureBox1.Visible = True
                        PictureBox1.ImageLocation = "link\6.png"
                        PictureBox1.Location = New Point(CType(c(0), Button).Location.X, CType(c(0), Button).Location.Y)
                        PictureBox1.Width = CType(c(0), Button).Width * (y5 - y1)
                        PictureBox1.Height = CType(c(0), Button).Height
                    End If
                    If (directing(x5, y5, x1, y1) = 1 And (directing(x5, y5, x3, y3) = 3)) Or ((directing(x5, y5, x1, y1) = 3 And (directing(x5, y5, x3, y3) = 1))) Then
                        PictureBox2.Visible = True
                        PictureBox2.ImageLocation = "link\7.png"
                        PictureBox2.Location = New Point(CType(g(0), Button).Location.X, CType(g(0), Button).Location.Y)
                    End If
                    If (directing(x5, y5, x1, y1) = 1 And (directing(x5, y5, x3, y3) = 4)) Or ((directing(x5, y5, x1, y1) = 4 And (directing(x5, y5, x3, y3) = 1))) Then
                        PictureBox2.Visible = True
                        PictureBox2.ImageLocation = "link\8.png"
                        PictureBox2.Location = New Point(CType(g(0), Button).Location.X, CType(g(0), Button).Location.Y)
                    End If
                    If ((directing(x5, y5, x1, y1) = 2 And (directing(x5, y5, x3, y3) = 3))) Or ((directing(x5, y5, x1, y1) = 3 And (directing(x5, y5, x3, y3) = 2))) Then
                        PictureBox2.Visible = True
                        PictureBox2.ImageLocation = "link\9.png"
                        PictureBox2.Location = New Point(CType(g(0), Button).Location.X, CType(g(0), Button).Location.Y)
                    End If
                    If (directing(x5, y5, x1, y1) = 2 And (directing(x5, y5, x3, y3) = 4)) Or ((directing(x5, y5, x1, y1) = 4 And (directing(x5, y5, x3, y3) = 2))) Then
                        PictureBox2.Visible = True
                        PictureBox2.ImageLocation = "link\10.png"
                        PictureBox2.Location = New Point(CType(g(0), Button).Location.X, CType(g(0), Button).Location.Y)
                    End If
                    If directing(x5, y5, x3, y3) = 2 Then
                        PictureBox3.Visible = True
                        PictureBox3.ImageLocation = "link\11.png"
                        PictureBox3.Location = New Point(CType(e(0), Button).Location.X, CType(e(0), Button).Location.Y + CType(e(0), Button).Height)
                        PictureBox3.Width = CType(e(0), Button).Width
                        PictureBox3.Height = CType(e(0), Button).Height * (x5 - x3 - 1)
                    End If
                    If directing(x5, y5, x3, y3) = 1 Then
                        PictureBox3.Visible = True
                        PictureBox3.ImageLocation = "link\11.png"
                        PictureBox3.Location = New Point(CType(g(0), Button).Location.X, CType(g(0), Button).Location.Y + CType(g(0), Button).Height)
                        PictureBox3.Width = CType(g(0), Button).Width
                        PictureBox3.Height = CType(g(0), Button).Height * (x3 - x5 - 1)
                    End If
                    If directing(x5, y5, x3, y3) = 3 Then
                        PictureBox3.Visible = True
                        PictureBox3.ImageLocation = "link\12.png"
                        PictureBox3.Location = New Point(CType(e(0), Button).Location.X + CType(e(0), Button).Width, CType(e(0), Button).Location.Y)
                        PictureBox3.Width = CType(e(0), Button).Width * (y5 - y3 - 1)
                        PictureBox3.Height = CType(e(0), Button).Height
                    End If
                    If directing(x5, y5, x3, y3) = 4 Then
                        PictureBox3.Visible = True
                        PictureBox3.ImageLocation = "link\12.png"
                        PictureBox3.Location = New Point(CType(g(0), Button).Location.X + CType(g(0), Button).Width, CType(g(0), Button).Location.Y)
                        PictureBox3.Width = CType(g(0), Button).Width * (y3 - y5 - 1)
                        PictureBox3.Height = CType(g(0), Button).Height
                    End If
                    If ((directing(x3, y3, x5, y5) = 2 And (directing(x3, y3, x4, y4) = 3))) Or ((directing(x3, y3, x5, y5) = 3 And (directing(x3, y3, x4, y4) = 2))) Then
                        PictureBox4.Visible = True
                        PictureBox4.ImageLocation = "link\9.png"
                        PictureBox4.Location = New Point(CType(e(0), Button).Location.X, CType(e(0), Button).Location.Y)
                    End If
                    If ((directing(x3, y3, x5, y5) = 2 And (directing(x3, y3, x4, y4) = 4))) Or ((directing(x3, y3, x5, y5) = 4 And (directing(x3, y3, x4, y4) = 2))) Then
                        PictureBox4.Visible = True
                        PictureBox4.ImageLocation = "link\10.png"
                        PictureBox4.Location = New Point(CType(e(0), Button).Location.X, CType(e(0), Button).Location.Y)
                    End If
                    If ((directing(x3, y3, x5, y5) = 1 And (directing(x3, y3, x4, y4) = 3))) Or ((directing(x3, y3, x5, y5) = 3 And (directing(x3, y3, x4, y4) = 1))) Then
                        PictureBox4.Visible = True
                        PictureBox4.ImageLocation = "link\7.png"
                        PictureBox4.Location = New Point(CType(e(0), Button).Location.X, CType(e(0), Button).Location.Y)
                    End If
                    If ((directing(x3, y3, x5, y5) = 1 And (directing(x3, y3, x4, y4) = 4))) Or ((directing(x3, y3, x5, y5) = 4 And (directing(x3, y3, x4, y4) = 1))) Then
                        PictureBox4.Visible = True
                        PictureBox4.ImageLocation = "link\8.png"
                        PictureBox4.Location = New Point(CType(e(0), Button).Location.X, CType(e(0), Button).Location.Y)
                    End If
                    If directing(x4, y4, x3, y3) = 2 Then
                        PictureBox5.Visible = True
                        PictureBox5.ImageLocation = "link\4.png"
                        PictureBox5.Location = New Point(CType(e(0), Button).Location.X, CType(e(0), Button).Location.Y + CType(e(0), Button).Height)
                        PictureBox5.Width = CType(e(0), Button).Width
                        PictureBox5.Height = CType(e(0), Button).Height * (x4 - x3)
                    End If
                    If directing(x4, y4, x3, y3) = 1 Then
                        PictureBox5.Visible = True
                        PictureBox5.ImageLocation = "link\3.png"
                        PictureBox5.Location = New Point(CType(f(0), Button).Location.X, CType(f(0), Button).Location.Y)
                        PictureBox5.Width = CType(f(0), Button).Width
                        PictureBox5.Height = CType(f(0), Button).Height * (x3 - x4)
                    End If
                    If directing(x4, y4, x3, y3) = 3 Then
                        PictureBox5.Visible = True
                        PictureBox5.ImageLocation = "link\5.png"
                        PictureBox5.Location = New Point(CType(e(0), Button).Location.X + CType(e(0), Button).Height, CType(e(0), Button).Location.Y)
                        PictureBox5.Width = CType(e(0), Button).Width * (y4 - y3)
                        PictureBox5.Height = CType(e(0), Button).Height
                    End If
                    If directing(x4, y4, x3, y3) = 4 Then
                        PictureBox5.Visible = True
                        PictureBox5.ImageLocation = "link\6.png"
                        PictureBox5.Location = New Point(CType(f(0), Button).Location.X, CType(f(0), Button).Location.Y)
                        PictureBox5.Width = CType(f(0), Button).Width * (y3 - y4)
                        PictureBox5.Height = CType(f(0), Button).Height
                    End If
                    counter = 0
                    Timer1.Start()
                    Debug.WriteLine("C" & x1 & "," & y1 & "," & x2 & "," & y2 & "," & x3 & "," & y3 & "," & x4 & "," & y4 & "," & x5 & "," & y5)
                    c_test += 1
                Else
                    'graphicsObject.DrawLine(pen_one, CType(c(0), Button).Location.X + 25, CType(c(0), Button).Location.Y + 25, CType(d(0), Button).Location.X + 25, CType(d(0), Button).Location.Y + 25)
                    'graphicsObject.DrawLine(pen_one, CType(d(0), Button).Location.X + 25, CType(d(0), Button).Location.Y + 25, CType(g(0), Button).Location.X + 25, CType(g(0), Button).Location.Y + 25)
                    'graphicsObject.DrawLine(pen_one, CType(g(0), Button).Location.X + 25, CType(g(0), Button).Location.Y + 25, CType(f(0), Button).Location.X + 25, CType(f(0), Button).Location.Y + 25)
                    If (directing(x1, y1, x2, y2) = 1) Then
                        PictureBox1.Visible = True
                        PictureBox1.ImageLocation = "link\3.png"
                        PictureBox1.Location = New Point(CType(c(0), Button).Location.X, CType(c(0), Button).Location.Y)
                        PictureBox1.Width = CType(c(0), Button).Width
                        PictureBox1.Height = CType(c(0), Button).Height * (x2 - x1)
                    End If
                    If (directing(x1, y1, x2, y2) = 2) Then
                        PictureBox1.Visible = True
                        PictureBox1.ImageLocation = "link\4.png"
                        PictureBox1.Location = New Point(CType(d(0), Button).Location.X, CType(d(0), Button).Location.Y + CType(d(0), Button).Height)
                        PictureBox1.Width = CType(d(0), Button).Width
                        PictureBox1.Height = CType(d(0), Button).Height * (x1 - x2)
                    End If
                    If (directing(x1, y1, x2, y2) = 3) Then
                        PictureBox1.Visible = True
                        PictureBox1.ImageLocation = "link\5.png"
                        PictureBox1.Location = New Point(CType(d(0), Button).Location.X + CType(d(0), Button).Width, CType(d(0), Button).Location.Y)
                        PictureBox1.Width = CType(d(0), Button).Width * (y1 - y2)
                        PictureBox1.Height = CType(d(0), Button).Height
                    End If
                    If (directing(x1, y1, x2, y2) = 4) Then
                        PictureBox1.Visible = True
                        PictureBox1.ImageLocation = "link\6.png"
                        PictureBox1.Location = New Point(CType(c(0), Button).Location.X, CType(c(0), Button).Location.Y)
                        PictureBox1.Width = CType(c(0), Button).Width * (y2 - y1)
                        PictureBox1.Height = CType(c(0), Button).Height
                    End If
                    If (directing(x2, y2, x1, y1) = 1 And (directing(x2, y2, x5, y5) = 3)) Or ((directing(x2, y2, x1, y1) = 3 And (directing(x2, y2, x5, y5) = 1))) Then
                        PictureBox2.Visible = True
                        PictureBox2.ImageLocation = "link\7.png"
                        PictureBox2.Location = New Point(CType(d(0), Button).Location.X, CType(d(0), Button).Location.Y)
                    End If
                    If (directing(x2, y2, x1, y1) = 1 And (directing(x2, y2, x5, y5) = 4)) Or ((directing(x2, y2, x1, y1) = 4 And (directing(x2, y2, x5, y5) = 1))) Then
                        PictureBox2.Visible = True
                        PictureBox2.ImageLocation = "link\8.png"
                        PictureBox2.Location = New Point(CType(d(0), Button).Location.X, CType(d(0), Button).Location.Y)
                    End If
                    If ((directing(x2, y2, x1, y1) = 2 And (directing(x2, y2, x5, y5) = 3))) Or ((directing(x2, y2, x1, y1) = 3 And (directing(x2, y2, x5, y5) = 2))) Then
                        PictureBox2.Visible = True
                        PictureBox2.ImageLocation = "link\9.png"
                        PictureBox2.Location = New Point(CType(d(0), Button).Location.X, CType(d(0), Button).Location.Y)
                    End If
                    If (directing(x2, y2, x1, y1) = 2 And (directing(x2, y2, x5, y5) = 4)) Or ((directing(x2, y2, x1, y1) = 4 And (directing(x2, y2, x5, y5) = 2))) Then
                        PictureBox2.Visible = True
                        PictureBox2.ImageLocation = "link\10.png"
                        PictureBox2.Location = New Point(CType(d(0), Button).Location.X, CType(d(0), Button).Location.Y)
                    End If
                    If directing(x2, y2, x5, y5) = 2 Then
                        PictureBox3.Visible = True
                        PictureBox3.ImageLocation = "link\11.png"
                        PictureBox3.Location = New Point(CType(g(0), Button).Location.X, CType(g(0), Button).Location.Y + CType(g(0), Button).Height)
                        PictureBox3.Width = CType(g(0), Button).Width
                        PictureBox3.Height = CType(g(0), Button).Height * (x2 - x5 - 1)
                    End If
                    If directing(x2, y2, x5, y5) = 1 Then
                        PictureBox3.Visible = True
                        PictureBox3.ImageLocation = "link\11.png"
                        PictureBox3.Location = New Point(CType(d(0), Button).Location.X, CType(d(0), Button).Location.Y + CType(d(0), Button).Height)
                        PictureBox3.Width = CType(d(0), Button).Width
                        PictureBox3.Height = CType(d(0), Button).Height * (x5 - x2 - 1)
                    End If
                    If directing(x2, y2, x5, y5) = 3 Then
                        PictureBox3.Visible = True
                        PictureBox3.ImageLocation = "link\12.png"
                        PictureBox3.Location = New Point(CType(g(0), Button).Location.X + CType(g(0), Button).Width, CType(g(0), Button).Location.Y)
                        PictureBox3.Width = CType(g(0), Button).Width * (y2 - y5 - 1)
                        PictureBox3.Height = CType(g(0), Button).Height
                    End If
                    If directing(x2, y2, x5, y5) = 4 Then
                        PictureBox3.Visible = True
                        PictureBox3.ImageLocation = "link\12.png"
                        PictureBox3.Location = New Point(CType(d(0), Button).Location.X + CType(d(0), Button).Width, CType(d(0), Button).Location.Y)
                        PictureBox3.Width = CType(d(0), Button).Width * (y5 - y2 - 1)
                        PictureBox3.Height = CType(d(0), Button).Height
                    End If
                    If ((directing(x5, y5, x2, y2) = 2 And (directing(x5, y5, x4, y4) = 3))) Or ((directing(x5, y5, x2, y2) = 3 And (directing(x5, y5, x4, y4) = 2))) Then
                        PictureBox4.Visible = True
                        PictureBox4.ImageLocation = "link\9.png"
                        PictureBox4.Location = New Point(CType(g(0), Button).Location.X, CType(g(0), Button).Location.Y)
                    End If
                    If ((directing(x5, y5, x2, y2) = 2 And (directing(x5, y5, x4, y4) = 4))) Or ((directing(x5, y5, x2, y2) = 4 And (directing(x5, y5, x4, y4) = 2))) Then
                        PictureBox4.Visible = True
                        PictureBox4.ImageLocation = "link\10.png"
                        PictureBox4.Location = New Point(CType(g(0), Button).Location.X, CType(g(0), Button).Location.Y)
                    End If
                    If ((directing(x5, y5, x2, y2) = 1 And (directing(x5, y5, x4, y4) = 3))) Or ((directing(x5, y5, x2, y2) = 3 And (directing(x5, y5, x4, y4) = 1))) Then
                        PictureBox4.Visible = True
                        PictureBox4.ImageLocation = "link\7.png"
                        PictureBox4.Location = New Point(CType(g(0), Button).Location.X, CType(g(0), Button).Location.Y)
                    End If
                    If ((directing(x5, y5, x2, y2) = 1 And (directing(x5, y5, x4, y4) = 4))) Or ((directing(x5, y5, x2, y2) = 4 And (directing(x5, y5, x4, y4) = 1))) Then
                        PictureBox4.Visible = True
                        PictureBox4.ImageLocation = "link\8.png"
                        PictureBox4.Location = New Point(CType(g(0), Button).Location.X, CType(g(0), Button).Location.Y)
                    End If
                    If directing(x4, y4, x5, y5) = 2 Then
                        PictureBox5.Visible = True
                        PictureBox5.ImageLocation = "link\4.png"
                        PictureBox5.Location = New Point(CType(g(0), Button).Location.X, CType(g(0), Button).Location.Y + CType(g(0), Button).Height)
                        PictureBox5.Width = CType(g(0), Button).Width
                        PictureBox5.Height = CType(g(0), Button).Height * (x4 - x5)
                    End If
                    If directing(x4, y4, x5, y5) = 1 Then
                        PictureBox5.Visible = True
                        PictureBox5.ImageLocation = "link\3.png"
                        PictureBox5.Location = New Point(CType(f(0), Button).Location.X, CType(f(0), Button).Location.Y)
                        PictureBox5.Width = CType(f(0), Button).Width
                        PictureBox5.Height = CType(f(0), Button).Height * (x5 - x4)
                    End If
                    If directing(x4, y4, x5, y5) = 3 Then
                        PictureBox5.Visible = True
                        PictureBox5.ImageLocation = "link\5.png"
                        PictureBox5.Location = New Point(CType(g(0), Button).Location.X + CType(g(0), Button).Height, CType(g(0), Button).Location.Y)
                        PictureBox5.Width = CType(g(0), Button).Width * (y4 - y5)
                        PictureBox5.Height = CType(g(0), Button).Height
                    End If
                    If directing(x4, y4, x5, y5) = 4 Then
                        PictureBox5.Visible = True
                        PictureBox5.ImageLocation = "link\6.png"
                        PictureBox5.Location = New Point(CType(f(0), Button).Location.X, CType(f(0), Button).Location.Y)
                        PictureBox5.Width = CType(f(0), Button).Width * (y5 - y4)
                        PictureBox5.Height = CType(f(0), Button).Height
                    End If
                    counter = 0
                    Timer1.Start()
                    Debug.WriteLine("D" & x1 & "," & y1 & "," & x2 & "," & y2 & "," & x3 & "," & y3 & "," & x4 & "," & y4 & "," & x5 & "," & y5)
                    d_test += 1
                End If
            End If
        Else
            'graphicsObject.DrawLine(pen_one, CType(c(0), Button).Location.X + 25, CType(c(0), Button).Location.Y + 25, CType(d(0), Button).Location.X + 25, CType(d(0), Button).Location.Y + 25)
            If (directing(x1, y1, x2, y2) = 1) Then
                PictureBox1.Visible = True
                PictureBox1.ImageLocation = "link\3.png"
                PictureBox1.Location = New Point(CType(c(0), Button).Location.X, CType(c(0), Button).Location.Y)
                PictureBox1.Width = CType(c(0), Button).Width
                PictureBox1.Height = CType(c(0), Button).Height * (x2 - x1)
            End If
            If (directing(x1, y1, x2, y2) = 2) Then
                PictureBox1.Visible = True
                PictureBox1.ImageLocation = "link\4.png"
                PictureBox1.Location = New Point(CType(d(0), Button).Location.X, CType(d(0), Button).Location.Y + CType(d(0), Button).Height)
                PictureBox1.Width = CType(d(0), Button).Width
                PictureBox1.Height = CType(d(0), Button).Height * (x1 - x2)
            End If
            If (directing(x1, y1, x2, y2) = 3) Then
                PictureBox1.Visible = True
                PictureBox1.ImageLocation = "link\5.png"
                PictureBox1.Location = New Point(CType(d(0), Button).Location.X + CType(d(0), Button).Width, CType(d(0), Button).Location.Y)
                PictureBox1.Width = CType(c(0), Button).Width * (y1 - y2)
                PictureBox1.Height = CType(c(0), Button).Height
            End If
            If (directing(x1, y1, x2, y2) = 4) Then
                PictureBox1.Visible = True
                PictureBox1.ImageLocation = "link\6.png"
                PictureBox1.Location = New Point(CType(c(0), Button).Location.X, CType(c(0), Button).Location.Y)
                PictureBox1.Width = CType(c(0), Button).Width * (y2 - y1)
                PictureBox1.Height = CType(c(0), Button).Height
            End If
            If (directing(x2, y2, x1, y1) = 1 And (directing(x2, y2, x3, y3) = 3)) Or ((directing(x2, y2, x1, y1) = 3 And (directing(x2, y2, x3, y3) = 1))) Then
                PictureBox2.Visible = True
                PictureBox2.ImageLocation = "link\7.png"
                PictureBox2.Location = New Point(CType(d(0), Button).Location.X, CType(d(0), Button).Location.Y)
            End If
            If (directing(x2, y2, x1, y1) = 1 And (directing(x2, y2, x3, y3) = 4)) Or ((directing(x2, y2, x1, y1) = 4 And (directing(x2, y2, x3, y3) = 1))) Then
                PictureBox2.Visible = True
                PictureBox2.ImageLocation = "link\8.png"
                PictureBox2.Location = New Point(CType(d(0), Button).Location.X, CType(d(0), Button).Location.Y)
            End If
            If ((directing(x2, y2, x1, y1) = 2 And (directing(x2, y2, x3, y3) = 3))) Or ((directing(x2, y2, x1, y1) = 3 And (directing(x2, y2, x3, y3) = 2))) Then
                PictureBox2.Visible = True
                PictureBox2.ImageLocation = "link\9.png"
                PictureBox2.Location = New Point(CType(d(0), Button).Location.X, CType(d(0), Button).Location.Y)
            End If
            If (directing(x2, y2, x1, y1) = 2 And (directing(x2, y2, x3, y3) = 4)) Or ((directing(x2, y2, x1, y1) = 4 And (directing(x2, y2, x3, y3) = 2))) Then
                PictureBox2.Visible = True
                PictureBox2.ImageLocation = "link\10.png"
                PictureBox2.Location = New Point(CType(d(0), Button).Location.X, CType(d(0), Button).Location.Y)
            End If
            If directing(x2, y2, x3, y3) = 2 Then
                PictureBox3.Visible = True
                PictureBox3.ImageLocation = "link\11.png"
                PictureBox3.Location = New Point(CType(e(0), Button).Location.X, CType(e(0), Button).Location.Y + CType(e(0), Button).Height)
                PictureBox3.Width = CType(e(0), Button).Width
                PictureBox3.Height = CType(e(0), Button).Height * (x2 - x3 - 1)
            End If
            If directing(x2, y2, x3, y3) = 1 Then
                PictureBox3.Visible = True
                PictureBox3.ImageLocation = "link\11.png"
                PictureBox3.Location = New Point(CType(d(0), Button).Location.X, CType(d(0), Button).Location.Y + CType(d(0), Button).Height)
                PictureBox3.Width = CType(e(0), Button).Width
                PictureBox3.Height = CType(e(0), Button).Height * (x3 - x2 - 1)
            End If
            If directing(x2, y2, x3, y3) = 3 Then
                PictureBox3.Visible = True
                PictureBox3.ImageLocation = "link\12.png"
                PictureBox3.Location = New Point(CType(e(0), Button).Location.X + CType(e(0), Button).Width, CType(e(0), Button).Location.Y)
                PictureBox3.Width = CType(e(0), Button).Width * (y2 - y3 - 1)
                PictureBox3.Height = CType(e(0), Button).Height
            End If
            If directing(x2, y2, x3, y3) = 4 Then
                PictureBox3.Visible = True
                PictureBox3.ImageLocation = "link\12.png"
                PictureBox3.Location = New Point(CType(d(0), Button).Location.X + CType(d(0), Button).Width, CType(d(0), Button).Location.Y)
                PictureBox3.Width = CType(d(0), Button).Width * (y3 - y2 - 1)
                PictureBox3.Height = CType(d(0), Button).Height
            End If
            If ((directing(x3, y3, x2, y2) = 2 And (directing(x3, y3, x4, y4) = 3))) Or ((directing(x3, y3, x2, y2) = 3 And (directing(x3, y3, x4, y4) = 2))) Then
                PictureBox4.Visible = True
                PictureBox4.ImageLocation = "link\9.png"
                PictureBox4.Location = New Point(CType(e(0), Button).Location.X, CType(e(0), Button).Location.Y)
            End If
            If ((directing(x3, y3, x2, y2) = 2 And (directing(x3, y3, x4, y4) = 4))) Or ((directing(x3, y3, x2, y2) = 4 And (directing(x3, y3, x4, y4) = 2))) Then
                PictureBox4.Visible = True
                PictureBox4.ImageLocation = "link\10.png"
                PictureBox4.Location = New Point(CType(e(0), Button).Location.X, CType(e(0), Button).Location.Y)
            End If
            If ((directing(x3, y3, x2, y2) = 1 And (directing(x3, y3, x4, y4) = 3))) Or ((directing(x3, y3, x2, y2) = 3 And (directing(x3, y3, x4, y4) = 1))) Then
                PictureBox4.Visible = True
                PictureBox4.ImageLocation = "link\7.png"
                PictureBox4.Location = New Point(CType(e(0), Button).Location.X, CType(e(0), Button).Location.Y)
            End If
            If ((directing(x3, y3, x2, y2) = 1 And (directing(x3, y3, x4, y4) = 4))) Or ((directing(x3, y3, x2, y2) = 4 And (directing(x3, y3, x4, y4) = 1))) Then
                PictureBox4.Visible = True
                PictureBox4.ImageLocation = "link\8.png"
                PictureBox4.Location = New Point(CType(e(0), Button).Location.X, CType(e(0), Button).Location.Y)
            End If
            If directing(x4, y4, x3, y3) = 2 Then
                PictureBox5.Visible = True
                PictureBox5.ImageLocation = "link\4.png"
                PictureBox5.Location = New Point(CType(e(0), Button).Location.X, CType(e(0), Button).Location.Y + CType(e(0), Button).Height)
                PictureBox5.Width = CType(e(0), Button).Width
                PictureBox5.Height = CType(e(0), Button).Height * (x4 - x3)
            End If
            If directing(x4, y4, x3, y3) = 1 Then
                PictureBox5.Visible = True
                PictureBox5.ImageLocation = "link\3.png"
                PictureBox5.Location = New Point(CType(f(0), Button).Location.X, CType(f(0), Button).Location.Y)
                PictureBox5.Width = CType(f(0), Button).Width
                PictureBox5.Height = CType(f(0), Button).Height * (x3 - x4)
            End If
            If directing(x4, y4, x3, y3) = 3 Then
                PictureBox5.Visible = True
                PictureBox5.ImageLocation = "link\5.png"
                PictureBox5.Location = New Point(CType(e(0), Button).Location.X + CType(e(0), Button).Height, CType(e(0), Button).Location.Y)
                PictureBox5.Width = CType(e(0), Button).Width * (y4 - y3)
                PictureBox5.Height = CType(e(0), Button).Height
            End If
            If directing(x4, y4, x3, y3) = 4 Then
                PictureBox5.Visible = True
                PictureBox5.ImageLocation = "link\6.png"
                PictureBox5.Location = New Point(CType(f(0), Button).Location.X, CType(f(0), Button).Location.Y)
                PictureBox5.Width = CType(f(0), Button).Width * (y3 - y4)
                PictureBox5.Height = CType(f(0), Button).Height
            End If
            'graphicsObject.DrawLine(pen_one, CType(d(0), Button).Location.X + 25, CType(d(0), Button).Location.Y + 25, CType(e(0), Button).Location.X + 25, CType(e(0), Button).Location.Y + 25)
            'graphicsObject.DrawLine(pen_one, CType(e(0), Button).Location.X + 25, CType(e(0), Button).Location.Y + 25, CType(f(0), Button).Location.X + 25, CType(f(0), Button).Location.Y + 25)
            Timer1.Start()
            counter = 0
            ' Debug.WriteLine("N" & x1 & "," & y1 & "," & x2 & "," & y2 & "," & x3 & "," & y3 & "," & x4 & "," & y4)
            n_test += 1
        End If
    End Sub

    Private Sub Button3_Click(sender As Object, e As EventArgs) Handles Button3.Click
        Timer2.Start()
        Timer3.Start()
    End Sub

    Private Sub Timer2_Tick(sender As Object, e As EventArgs) Handles Timer2.Tick
        show_counter += 1
        For i = 0 To ar.GetUpperBound(0)
            For j = 0 To ar.GetUpperBound(1)
                If ar(i, j) <> 0 Then
                    For k = 0 To ar.GetUpperBound(0)
                        For l = 0 To ar.GetUpperBound(1)
                            If (ar(i, j) = ar(k, l)) And ((i <> k) Or (j <> l)) And (finallink(i, j, k, l) = True) Then
                                'Debug.WriteLine(i & "," & j & "," & k & "," & l)
                                Dim c() As Control = Me.Controls.Find(name_store(i, j), False)
                                Dim d() As Control = Me.Controls.Find(name_store(k, l), False)
                                If show_counter Mod 2 = 0 Then
                                    If InStr(CType(c(0), Button).ImageKey, "m") = 0 Then CType(c(0), Button).ImageKey = Replace(CType(c(0), Button).ImageKey, ".", "m.")
                                    If InStr(CType(d(0), Button).ImageKey, "m") = 0 Then CType(d(0), Button).ImageKey = Replace(CType(d(0), Button).ImageKey, ".", "m.")
                                    Exit Sub
                                End If
                                If show_counter Mod 2 = 1 Then
                                    CType(c(0), Button).Visible = False
                                    ar(i, j) = 0
                                    CType(d(0), Button).Visible = False
                                    ar(k, l) = 0
                                    'Debug.WriteLine(CType(c(0), Button).Tag)
                                    'Debug.WriteLine(CType(c(0), Button).Location)
                                    'Debug.WriteLine(CType(d(0), Button).Tag)
                                    'Debug.WriteLine(CType(d(0), Button).Location)
                                    combo += 1
                                    combo_time_counter = 0
                                    combobox()
                                    Select Case draw_judge
                                        Case 1
                                            draw_line_one(i, j, k, l)
                                        Case 2
                                            draw_line_two(i, j, draw_coord_store(0), draw_coord_store(1), k, l)
                                        Case 3
                                            draw_line_three(i, j, draw_coord_store(2), draw_coord_store(3), draw_coord_store(4), draw_coord_store(5), k, l, draw_coord_store(6), draw_coord_store(7))
                                    End Select
                                    If success_test() = True Then
                                        Debug.WriteLine("regenerate beginning, timer3 stopped")
                                        Timer3.Stop()
                                        clean_button_memory()
                                        generate()
                                        Debug.WriteLine("regenerate finished, timer3 started")
                                        Timer3.Start()
                                    End If
                                    Exit Sub
                                End If
                            End If
                        Next
                    Next
                End If
            Next
        Next
    End Sub

    Private Sub Button4_Click(sender As Object, e As EventArgs) Handles Button4.Click
        Timer2.Stop()
        Timer3.Stop()
    End Sub

    Sub rearrange()
        Dim n As Integer
        Dim m As Integer
        Dim tmp As Integer
        Dim k As Integer
        Dim l As Integer
        Dim counter As Integer
        For i = 0 To ar.GetUpperBound(0)
            For j = 0 To ar.GetUpperBound(1)
                If ar(i, j) <> 0 Then counter += 1
            Next
        Next
        For h = 0 To (counter * 3)
            Do
                Randomize()
                n = Int(Rnd() * (ar.GetUpperBound(0) + 1))
                m = Int(Rnd() * (ar.GetUpperBound(1) + 1))
            Loop Until ar(n, m) <> 0
            tmp = ar(n, m)
            Do
                Randomize()
                k = Int(Rnd() * (ar.GetUpperBound(0) + 1))
                l = Int(Rnd() * (ar.GetUpperBound(1) + 1))
            Loop Until ar(k, l) <> 0
            ar(n, m) = ar(k, l)
            ar(k, l) = tmp
        Next
        clean_button_memory()
        button_build()
    End Sub

    

    Sub clean_button_memory()
        For i = 0 To ar.GetUpperBound(0)
            For j = 0 To ar.GetUpperBound(1)
                Dim c() As Control = Me.Controls.Find(name_store(i, j), False)
                Controls.Remove(CType(c(0), Button))
            Next
        Next
    End Sub

    'Sub judge_solution_possible_and_rearrange()
    '    If judge_solution_possible() = False Then
    '        rearrange()
    '    End If
    'End Sub

    Function judge_solution_possible() As Boolean
        For i = 0 To ar.GetUpperBound(0)
            For j = 0 To ar.GetUpperBound(1)
                If ar(i, j) <> 0 Then
                    For k = 0 To ar.GetUpperBound(0)
                        For l = 0 To ar.GetUpperBound(1)
                            If (ar(i, j) = ar(k, l)) And ((i <> k) Or (j <> l)) And (finallink(i, j, k, l) = True) Then Return True
                        Next
                    Next
                End If
            Next
        Next
        Return False
    End Function

    Private Sub Timer3_Tick(sender As Object, e As EventArgs) Handles Timer3.Tick


        Do While judge_solution_possible() = False
            rearrange()
        Loop

    End Sub

    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        generate()
        Timer4.Start()
    End Sub

    Function directing(ByVal x1 As Integer, ByVal y1 As Integer, ByVal x0 As Integer, ByVal y0 As Integer) As Integer
        If x1 = x0 Then
            If y1 < y0 Then Return 4
            If y1 > y0 Then Return 3
        End If
        If y1 = y0 Then
            If x1 < x0 Then Return 1
            If x1 > x0 Then Return 2
        End If
        Return -1
    End Function

    Private Sub Timer4_Tick(sender As Object, e As EventArgs) Handles Timer4.Tick
        combo_time_counter += 1
        If combo_time_counter >= 20 Then
            combo = 0
            combobox()
        End If
    End Sub

    Private Sub PictureBox6_MouseHover(sender As Object, e As EventArgs) Handles PictureBox6.MouseHover
        PictureBox6.Image = WindowsApplication1.My.Resources.Resources.hint_1
    End Sub

    Private Sub PictureBox6_MouseLeave(sender As Object, e As EventArgs) Handles PictureBox6.MouseLeave
        PictureBox6.Image = WindowsApplication1.My.Resources.Resources.hint
    End Sub

    Private Sub PictureBox7_MouseHover(sender As Object, e As EventArgs) Handles PictureBox7.MouseHover
        PictureBox7.Image = WindowsApplication1.My.Resources.Resources.REFRESH_1
    End Sub

    Private Sub PictureBox7_MouseLeave(sender As Object, e As EventArgs) Handles PictureBox7.MouseLeave
        PictureBox7.Image = WindowsApplication1.My.Resources.Resources.REFRESH
    End Sub

    Private Sub PictureBox6_Click(sender As Object, e As EventArgs) Handles PictureBox6.Click
        For i = 0 To ar.GetUpperBound(0)
            For j = 0 To ar.GetUpperBound(1)
                If ar(i, j) <> 0 Then
                    For k = 0 To ar.GetUpperBound(0)
                        For l = 0 To ar.GetUpperBound(1)
                            If (ar(i, j) = ar(k, l)) And ((i <> k) Or (j <> l)) And (finallink(i, j, k, l) = True) Then
                                Debug.WriteLine(i & "," & j & "," & k & "," & l)
                                Dim c() As Control = Me.Controls.Find(name_store(i, j), False)
                                If InStr(CType(c(0), Button).ImageKey, "l") <> 0 Then CType(c(0), Button).ImageKey = Replace(CType(c(0), Button).ImageKey, CType(c(0), Button).ImageKey.Substring(CType(c(0), Button).ImageKey.Length - 6, 6), "m.png")
                                If InStr(CType(c(0), Button).ImageKey, "m") = 0 Then CType(c(0), Button).ImageKey = Replace(CType(c(0), Button).ImageKey, ".", "m.")
                                Dim d() As Control = Me.Controls.Find(name_store(k, l), False)
                                If InStr(CType(d(0), Button).ImageKey, "l") <> 0 Then CType(d(0), Button).ImageKey = Replace(CType(d(0), Button).ImageKey, CType(d(0), Button).ImageKey.Substring(CType(d(0), Button).ImageKey.Length - 6, 6), "m.png")
                                If InStr(CType(d(0), Button).ImageKey, "m") = 0 Then CType(d(0), Button).ImageKey = Replace(CType(d(0), Button).ImageKey, ".", "m.")
                                Exit Sub
                            End If
                        Next
                    Next
                End If
            Next
        Next

    End Sub

    Private Sub PictureBox7_Click(sender As Object, e As EventArgs) Handles PictureBox7.Click
        rearrange()
    End Sub
End Class
