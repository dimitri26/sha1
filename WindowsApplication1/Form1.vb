Public Class Form1

    '5 heshey
    Dim h0, h1, h2, h3, h4 As UInt32



    Dim a, b, c, d, ee As UInt32

    'vsego 16 elementov
    Dim MyIn() As UInt32

    'vsego 80 elementov
    Dim W() As UInt32

    'vspomogatel'nye konstanty
    Const k1 As UInt32 = &H5A827999
    Const k2 As UInt32 = &H6ED9EBA1
    Const k3 As UInt32 = 2400959708     '&H8F1BBCDC 
    Const k4 As UInt32 = 3395469782     '&HCA62C1D6


	

    'znachenije funktsii
    Dim F As UInt32




  


    Sub Init()

        'nachal'naja initsializatsija hesh znacheny


        h0 = &H67452301
        h1 = 4023233417     '&HEFCDAB89
        h2 = 2562383102     '&H98BADCFE
        h3 = &H10325476
        h4 = 3285377520     '&HC3D2E1F0


    End Sub


    Sub MainStep()


       






        'vhodjashy block 512bit est' 16 32bit slov
        'pervye 16 W ot 0 do 15 est' vhodjashy block

        '0-79
        ReDim W(79)

        For Cou As Int32 = 0 To 15
            W(Cou) = MyIn(Cou)
        Next

        'generiruem eshe 64 elementa massiva ot 16 do 79
        For Cou As Int32 = 16 To 79
            W(Cou) = W(Cou - 3) Xor W(Cou - 8) Xor W(Cou - 14) Xor W(Cou - 16)
            W(Cou) = LeftRotate(W(Cou), 1)
        Next


        a = h0
        b = h1
        c = h2
        d = h3
        ee = h4




        Dim Temp As Int64


        For Cou As Int32 = 0 To 19

            F = (b And c) Or ((Not b) And d)

            'Temp = LeftRotate(a, 5) + F + ee + k1 + W(Cou)
            Temp = LeftRotate(a, 5)
            Temp += F
            Temp += ee
            Temp += k1
            Temp += W(Cou)
            Temp = Temp Mod (2 ^ 32)


            ee = d
            d = c
            c = LeftRotate(b, 30)
            b = a
            a = Temp

        Next


        For Cou As Int32 = 20 To 39

            F = b Xor c Xor d

            'Temp = LeftRotate(a, 5) + F + ee + k2 + W(Cou)
            Temp = LeftRotate(a, 5)
            Temp += F
            Temp += ee
            Temp += k2
            Temp += W(Cou)
            Temp = Temp Mod (2 ^ 32)



            ee = d
            d = c
            c = LeftRotate(b, 30)
            b = a
            a = Temp

        Next


        For Cou As Int32 = 40 To 59

            F = (b And c) Or (b And d) Or (c And d)

            'Temp = LeftRotate(a, 5) + F + ee + k3 + W(Cou)
            Temp = LeftRotate(a, 5)
            Temp += F
            Temp += ee
            Temp += k3
            Temp += W(Cou)
            Temp = Temp Mod (2 ^ 32)



            ee = d
            d = c
            c = LeftRotate(b, 30)
            b = a
            a = Temp

        Next


        For Cou As Int32 = 60 To 79

            F = b Xor c Xor d

            'Temp = LeftRotate(a, 5) + F + ee + k4 + W(Cou)
            Temp = LeftRotate(a, 5)
            Temp += F
            Temp += ee
            Temp += k4
            Temp += W(Cou)
            Temp = Temp Mod (2 ^ 32)



            ee = d
            d = c
            c = LeftRotate(b, 30)
            b = a
            a = Temp

        Next


        'dobavljaem hesh znachenija etoy chasti k rezul'tatu

        Temp = h0
        Temp += a
        Temp = Temp Mod (2 ^ 32)
        h0 = Temp

        Temp = h1
        Temp += b
        Temp = Temp Mod (2 ^ 32)
        h1 = Temp

        Temp = h2
        Temp += c
        Temp = Temp Mod (2 ^ 32)
        h2 = Temp

        Temp = h3
        Temp += d
        Temp = Temp Mod (2 ^ 32)
        h3 = Temp

        Temp = h4
        Temp += ee
        Temp = Temp Mod (2 ^ 32)
        h4 = Temp



        'itogo nash hesh est' h0 append h1 append h2 append h3 append h4

        TextBox2.Text = Hex(h0) & "  " & Hex(h1) & "  " & Hex(h2) & "  " & Hex(h3) & "  " & Hex(h4)


        'proizvodnaja beretsja po vsevozmozhnym napravlenijam
        'takzhe vazhno uchest' otlichie simvola Kronekkera ot bolee slaboy realizatsii


    End Sub


    Function LeftRotate(ByVal Value As UInt32, ByVal Bits As Int32) As UInt32
        'tsiklichesky sdvig vlevo
        '((value) << (bits)) | ((value) >> (32 - (bits))

        Dim y1, y2, y3 As UInt32

        y1 = Value << Bits
        y2 = Value >> (32 - Bits)

        y3 = y1 Or y2

        Return y3

    End Function




    Private Sub Button1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button1.Click

        Dim MyInString As String


        MyInString = TextBox1.Text


        'razbivaem MyInString na bloki po 512bit


        'Исходное сообщение разбивается на блоки по 512 бит в каждом. 
        'Последний блок дополняется до длины, кратной 512 бит. 
        'Сначала добавляется 1, а потом нули, чтобы длина блока стала равной (512 - 64 = 448) бит. 
        'В оставшиеся 64 бита записывается длина исходного сообщения в битах. 
        'Если последний блок имеет длину более 448, но менее 512 бит, 
        'то дополнение выполняется следующим образом: сначала добавляется 1, 
        'затем нули вплоть до конца 512-битного блока; 
        'после этого создается ещё один 512-битный блок, 
        'который заполняется вплоть до 448 бит нулями, 
        'после чего в оставшиеся 64 бита записывается длина исходного сообщения в битах. 
        'Дополнение последнего блока осуществляется всегда, 
        'даже если сообщение уже имеет нужную длину.


        'professional dmitry.shishlov@gmail.com
        '

        '''Dim MagicKEY As String = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"     '32+4 symbols  itogo 36 byta

        '''Dim WebSocketKey As String ' 16 byte random + 8 v kontse tipo ==   itogo 24 byta

        '''Dim SSS As String

        '''SSS = WebSocketKey & MagicKEY '36 + 24 = 60 t.e. 480 bit


        'dmitry shishlov @gmail.com

        Dim MyChar() As Char

        MyChar = MyInString.ToCharArray


        Dim MyInStringLength As Int32
        MyInStringLength = MyInString.Length

        Dim MyInStringLengthBit As Int32
        MyInStringLengthBit = MyInString.Length * 8    'dlina v bitah



        Dim Ostatok As Int32
        'Ostatok = MyInStringLengthBit Mod 512
        Ostatok = MyInStringLengthBit And 511


        Dim KoVoBlokov As Int32
        KoVoBlokov = (MyInStringLengthBit >> 9) + 1  'delenie natselo na 512


        Dim MyAsc() As Byte
        ReDim MyAsc(MyChar.Length - 1)

        For Cou As Int32 = 0 To (MyChar.Length - 1)
            MyAsc(Cou) = Asc(MyChar(Cou))
        Next



        If Ostatok > 448 Then
            KoVoBlokov += 1
        End If

        ReDim Preserve MyAsc(KoVoBlokov * 16 * 4 - 1)


        'dobavljaetsja 1
        MyAsc(MyInStringLength) = 128     ' 1000 0000 bin = 128 dec = 80 hex

        'v poslednie 64 bita  t.e. 8 byte zapisyvaem MyInStringLengthBit

        MyAsc(MyAsc.Length - 4) = (MyInStringLengthBit And &HFF000000) >> 24
        MyAsc(MyAsc.Length - 3) = (MyInStringLengthBit And &HFF0000) >> 16
        MyAsc(MyAsc.Length - 2) = (MyInStringLengthBit And &HFF00) >> 8
        MyAsc(MyAsc.Length - 1) = MyInStringLengthBit And &HFF


      

        ReDim MyIn(MyAsc.Length / 4 - 1)


        Call Init()




        Dim Cou1 As Int32 = 0
        Dim x1, x2, x3, x4 As UInt32



        For Cou As Int32 = 0 To (MyAsc.Length / 4 - 1)

            '####################################
            x1 = MyAsc(Cou * 4 + 0)
            x1 = x1 << 24

            x2 = MyAsc(Cou * 4 + 1)
            x2 = x2 << 16

            x3 = MyAsc(Cou * 4 + 2)
            x3 = x3 << 8

            x4 = MyAsc(Cou * 4 + 3)

            MyIn(Cou1) = x1 Or x2 Or x3 Or x4
            '#####################################

            Cou1 += 1
            If Cou1 = 16 Then
                Cou1 = 0
                Call MainStep()
            End If

        Next





    End Sub

  
    
End Class
