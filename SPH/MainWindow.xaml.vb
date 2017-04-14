Imports System.Threading.Tasks

Class MainWindow

    Public GRAVITY As Double = 0.0 ' 1.0
    Public VISCOSITY As Double = 4.0
    Public PRESSURE As Double = 4.0

    Public G_CONST As Double = 0.02

    Public ellipses As New List(Of Rectangle)
    Public particles As New List(Of Particle)

    Public scale As Double = 2.0

    Public left_down As Boolean = False
    Public right_down As Boolean = False
    Public r_down As Boolean = False
    Public rect_drag_point As New Point(0, 0)


    Public sim_running As Boolean = False

    Dim density_tasks As Integer = 0
    Dim force_tasks As Integer = 0

    Dim density_sl As New Object
    Dim force_sl As New Object

    Public Sub do_denstity_calc_task(ByVal i As Integer)
        Dim p As Particle = particles(i)

        For j As Integer = i To particles.Count - 1
            Dim q As Particle = particles(j)

            Dim d_x As Double = p.pos_x - q.pos_x
            Dim d_y As Double = p.pos_y - q.pos_y

            Dim w As Double = (d_x * d_x) + (d_y * d_y)

            If w < 4 Then
                Dim w2 As Double = Math.Sqrt(w) / 2 - 1

                p.density += (w2 * w2)
                q.density += (w2 * w2)
            End If
        Next

        SyncLock density_sl
            density_tasks += 1
        End SyncLock
    End Sub

    Public Sub density_calc_task()
        For i As Integer = 0 To particles.Count - 1
            Dim p As Particle = particles(i)

            If p.wallflag Then
                p.density = 9
            Else
                p.density = 0
            End If
        Next

        density_tasks = 0

        Dim num_tasks As Integer = 0
        Dim tf As New TaskFactory
        For i As Integer = 0 To particles.Count - 1
            Dim i_ As Integer = i
            tf.StartNew(Sub() do_denstity_calc_task(i_))

            num_tasks += 1
        Next

        While density_tasks <> num_tasks

        End While
    End Sub

    Public Sub density_calc()
        For i As Integer = 0 To particles.Count - 1
            Dim p As Particle = particles(i)

            If p.wallflag Then
                p.density = 9
            Else
                p.density = 0.1
            End If
        Next

        For i As Integer = 0 To particles.Count - 1
            Dim p As Particle = particles(i)

            For j As Integer = i + 1 To particles.Count - 1
                Dim q As Particle = particles(j)

                Dim d_x As Double = p.pos_x - q.pos_x
                Dim d_y As Double = p.pos_y - q.pos_y

                Dim w As Double = (d_x * d_x) + (d_y * d_y)

                If w < 4 Then
                    Dim w2 As Double = Math.Sqrt(w) / 2 - 1

                    p.density += (w2 * w2)
                    q.density += (w2 * w2)
                End If
            Next
        Next
    End Sub

    Public Sub do_force_calc_task(ByVal i As Integer)
        Dim p As Particle = particles(i)

        For j As Integer = i + 1 To particles.Count - 1
            Dim q As Particle = particles(j)

            Dim d_x As Double = p.pos_x - q.pos_x
            Dim d_y As Double = p.pos_y - q.pos_y

            Dim w As Double = (d_x * d_x) + (d_y * d_y)

            If w < 4 Then
                Dim w2 As Double = Math.Sqrt(w) / 2 - 1

                p.force_x += (d_x * (3 - p.density - q.density) * PRESSURE + p.vel_x * VISCOSITY - q.vel_x * VISCOSITY) * w2 / p.density
                p.force_y += (d_y * (3 - p.density - q.density) * PRESSURE + p.vel_y * VISCOSITY - q.vel_y * VISCOSITY) * w2 / p.density

                q.force_x += (-d_x * (3 - q.density - p.density) * PRESSURE + q.vel_x * VISCOSITY - p.vel_x * VISCOSITY) * w2 / q.density
                q.force_y += (-d_y * (3 - q.density - p.density) * PRESSURE + q.vel_y * VISCOSITY - p.vel_y * VISCOSITY) * w2 / q.density
            End If

            If w > 1 And Not q.wallflag And Not p.wallflag Then
                Dim force_amount As Double = G_CONST * ((p.density * q.density) / w)
                p.force_x -= (d_x / Math.Sqrt(w)) * force_amount
                p.force_y -= (d_y / Math.Sqrt(w)) * force_amount

                q.force_x -= (-d_x / Math.Sqrt(w)) * force_amount
                q.force_y -= (-d_y / Math.Sqrt(w)) * force_amount
            End If
        Next

        SyncLock force_sl
            force_tasks += 1
        End SyncLock
    End Sub

    Public Sub force_calc_task()
        For i As Integer = 0 To particles.Count - 1
            Dim p As Particle = particles(i)

            p.force_x = 0
            p.force_y = GRAVITY
        Next

        force_tasks = 0

        Dim num_tasks As Integer = 0
        Dim tf As New TaskFactory
        For i As Integer = 0 To particles.Count - 1
            Dim i_ As Integer = i
            tf.StartNew(Sub() do_force_calc_task(i_))
            num_tasks += 1
        Next

        While force_tasks <> num_tasks

        End While
    End Sub

    Public Sub force_calc()
        For i As Integer = 0 To particles.Count - 1
            Dim p As Particle = particles(i)

            p.force_x = 0
            p.force_y = GRAVITY
        Next

        For i As Integer = 0 To particles.Count - 1
            Dim p As Particle = particles(i)

            For j As Integer = i + 1 To particles.Count - 1
                Dim q As Particle = particles(j)

                Dim d_x As Double = p.pos_x - q.pos_x
                Dim d_y As Double = p.pos_y - q.pos_y

                Dim w As Double = (d_x * d_x) + (d_y * d_y)

                If w < 4 Then
                    Dim w2 As Double = Math.Sqrt(w) / 2 - 1

                    p.force_x += (d_x * (3 - p.density - q.density) * PRESSURE + p.vel_x * VISCOSITY - q.vel_x * VISCOSITY) * w2 / p.density
                    p.force_y += (d_y * (3 - p.density - q.density) * PRESSURE + p.vel_y * VISCOSITY - q.vel_y * VISCOSITY) * w2 / p.density

                    q.force_x += (-d_x * (3 - q.density - p.density) * PRESSURE + q.vel_x * VISCOSITY - p.vel_x * VISCOSITY) * w2 / q.density
                    q.force_y += (-d_y * (3 - q.density - p.density) * PRESSURE + q.vel_y * VISCOSITY - p.vel_y * VISCOSITY) * w2 / q.density
                End If

                If w > 1 And Not q.wallflag Then
                    Dim force_amount As Double = G_CONST * ((p.density * q.density) / w)
                    p.force_x -= (d_x / Math.Sqrt(w)) * force_amount
                    p.force_y -= (d_y / Math.Sqrt(w)) * force_amount

                    q.force_x -= (-d_x / Math.Sqrt(w)) * force_amount
                    q.force_y -= (-d_y / Math.Sqrt(w)) * force_amount
                End If

            Next
        Next
    End Sub

    Public Sub update_sim()
        For i As Integer = 0 To particles.Count - 1
            Dim p As Particle = particles(i)

            If Not p.wallflag Then
                p.vel_x += p.force_x / 20.0
                p.vel_y += p.force_y / 20.0

                p.pos_x += p.vel_x
                p.pos_y += p.vel_y
            End If
        Next
    End Sub

    Public Sub update_display()
        'main_canvas.Children.Clear()

        For i As Integer = 0 To particles.Count - 1
            Dim p As Particle = particles(i)

            Dim e As Rectangle = ellipses(i)
            Dim vis_scale = scale * 2
            e.Width = vis_scale
            e.Height = vis_scale

            Dim brightness As Byte = Math.Min(p.density * 25, 255)
            e.Fill = New SolidColorBrush(Color.FromRgb(brightness, 0, 0))

            Canvas.SetLeft(e, (p.pos_x * scale) - (scale / 2.0))
            Canvas.SetTop(e, (p.pos_y * scale) - (scale / 2.0))
            'main_canvas.Children.Add(e)
        Next
    End Sub

    Public Sub sim_thread()
        While sim_running
            density_calc_task()
            force_calc_task()
            Dispatcher.Invoke(Sub() update_display())
            'update_display()
            update_sim()

            System.Threading.Thread.Sleep(1000 / 60)
        End While
    End Sub

    Private Sub Window_KeyDown(ByVal sender As System.Object, ByVal e As System.Windows.Input.KeyEventArgs) Handles MyBase.KeyDown
        If e.Key = Key.R Then
            If Not r_down Then
                r_down = True
                rect_drag_point = Mouse.GetPosition(main_canvas)
            End If
        ElseIf e.Key = Key.Space And Not sim_running Then
            sim_running = True
            Dim t As New System.Threading.Thread(AddressOf sim_thread)
            t.Start()
            'density_calc_task()
            'force_calc_task()
            'update_display()
            'update_sim()
        ElseIf e.Key = Key.Space And sim_running Then
            sim_running = False
        End If
    End Sub
    Private Sub Window_KeyUp(ByVal sender As System.Object, ByVal e As System.Windows.Input.KeyEventArgs) Handles MyBase.KeyUp
        If e.Key = Key.R Then
            If r_down Then
                r_down = False
                Dim pos As Point = Mouse.GetPosition(main_canvas)

                For x As Double = rect_drag_point.X To pos.X Step scale
                    For y As Double = rect_drag_point.Y To pos.Y Step scale

                        Dim point_okay As Boolean = True

                        For i As Integer = 0 To particles.Count - 1
                            Dim p As Particle = particles(i)

                            Dim d_x As Double = p.pos_x - (x / scale)
                            Dim d_y As Double = p.pos_y - (y / scale)

                            Dim w As Double = (d_x * d_x) + (d_y * d_y)
                            Dim w2 As Double = Math.Sqrt(w) / 2

                            If w2 < 0.4 Then
                                point_okay = False
                                Exit For
                            End If
                        Next
                        If point_okay Then
                            Dim newp As New Particle(x / scale, y / scale, False)
                            particles.Add(newp)

                            Dim el As New Rectangle
                            ellipses.Add(el)
                            main_canvas.Children.Add(el)
                        End If

                    Next
                Next
                update_display()
            End If
        End If
    End Sub

    Private Sub main_canvas_MouseMove(ByVal sender As System.Object, ByVal e As System.Windows.Input.MouseEventArgs) Handles main_canvas.MouseMove
        If left_down Or right_down Then
            Dim pos As Point = Mouse.GetPosition(main_canvas)
            pos.X /= scale
            pos.Y /= scale

            Dim point_okay As Boolean = True

            For i As Integer = 0 To particles.Count - 1
                Dim p As Particle = particles(i)

                Dim d_x As Double = p.pos_x - pos.X
                Dim d_y As Double = p.pos_y - pos.Y

                Dim w As Double = (d_x * d_x) + (d_y * d_y)
                Dim w2 As Double = Math.Sqrt(w) / 2

                If w2 < 0.4 Then
                    point_okay = False
                    Exit For
                End If
            Next

            If point_okay Then
                If left_down Then
                    Dim p As New Particle(pos.X, pos.Y, False)
                    particles.Add(p)
                    Dim el As New Rectangle
                    ellipses.Add(el)
                    main_canvas.Children.Add(el)
                    update_display()
                ElseIf right_down Then
                    Dim p As New Particle(pos.X, pos.Y, True)
                    particles.Add(p)
                    Dim el As New Rectangle
                    ellipses.Add(el)
                    main_canvas.Children.Add(el)
                    update_display()
                End If
            End If
        End If
    End Sub
    Private Sub main_canvas_MouseLeftButtonDown(ByVal sender As System.Object, ByVal e As System.Windows.Input.MouseButtonEventArgs) Handles main_canvas.MouseLeftButtonDown
        left_down = True
    End Sub
    Private Sub main_canvas_MouseRightButtonDown(ByVal sender As System.Object, ByVal e As System.Windows.Input.MouseButtonEventArgs) Handles main_canvas.MouseRightButtonDown
        right_down = True
    End Sub
    Private Sub main_canvas_MouseLeftButtonUp(ByVal sender As System.Object, ByVal e As System.Windows.Input.MouseButtonEventArgs) Handles main_canvas.MouseLeftButtonUp
        left_down = False
    End Sub
    Private Sub main_canvas_MouseRightButtonUp(ByVal sender As System.Object, ByVal e As System.Windows.Input.MouseButtonEventArgs) Handles main_canvas.MouseRightButtonUp
        right_down = False
    End Sub


End Class

Public Class Particle
    Public pos_x As Double = 0
    Public pos_y As Double = 0

    Public vel_x As Double = 0
    Public vel_y As Double = 0

    Public force_x As Double = 0
    Public force_y As Double = 0

    Public density As Double = 0

    Public wallflag As Boolean = False

    Public Sub New(ByVal pos_x As Double, ByVal pos_y As Double, ByVal wallflag As Boolean)
        Me.pos_x = pos_x
        Me.pos_y = pos_y

        Me.wallflag = wallflag
    End Sub
End Class
