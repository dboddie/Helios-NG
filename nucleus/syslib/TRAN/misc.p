�
..1.A:��`�`�._SysLib_Init�_SysLib_Init �
._SysLib_Init:�`�r0�modnum��r0�@_Objects�_Objects�r�.InitList�r0�@_Streams�_Streams�r�.InitList�Ar0�@_ObjectLock�_ObjectLock�r�.InitSemaphore�Ar0�@_StreamLock�_StreamLock�r�.InitSemaphore�Ar0�@_BufLock�_BufLock�r�.InitSemaphore�@r0�@_IOCBuf�_IOCBuf�@p�(..dataseg.A+4)�r0�@_PipeList�_PipeList�r�.InitList�Ar0�@_PipeLock�_PipeLock�r�.InitSemaphore��"��
..12.A:��`�`�.NewMsgBuf�
NewMsgBuf �
.NewMsgBuf:�	`�v0�v1�s�modnum�҄(..13.A-2)�!��" @w��..5.A�" @�
..5.A:�s�@_BufLock�_BufLock��.Wait�w" ��..6.A�s�@_IOCBuf�_IOCBuf�..6.A�s�@_IOCBuf�_IOCBuf��p0s�@_IOCBuf�_IOCBuf�..9.A
..6.A:�w"&�a ��.Malloc��p��..10.A�s�@_BufLock�_BufLock��.Signal�q0�.Delay�..5.A
..10.A:�r�(..dataseg.A+4)��r�(..dataseg.A+4)
..9.A:�wp�p"�p�p&�p�s�@_BufLock�_BufLock��.Signal�p�"��
..13.A:�@B �
..20.A:��`�`�.FreeMsgBuf�FreeMsgBuf �
.FreeMsgBuf:�	`�u0�u1�r�modnum��v�p��..15.A��"�
..15.A:�p7" @��..17.A�p�.Free�q�(..dataseg.A+4)��q�(..dataseg.A+4)�..19.A
..17.A:�r�@_BufLock�_BufLock��.Wait�r�@_IOCBuf�_IOCBuf�p�pr�@_IOCBuf�_IOCBuf�r�@_BufLock�_BufLock��.Signal
..19.A:��"��
..30.A:��`�`�.PreallocMsgBufs�PreallocMsgBufs �
.PreallocMsgBufs:�`�@�@��
..22.A:�t`�t��@��..27.A�@s�.NewMsgBuf��p���..27.A�qp�pр..22.A�
..27.A:�p�..28.A�p�q0�qs�.FreeMsgBuf�..27.A�
..28.A:��"��
..40.A:��`�`�.CopyObject�CopyObject �
.CopyObject:�`��(..41.A-2)�
!��@�@�@wv�.CheckObject�..32.A�@�"�
..32.A:�w3t0$��..34.A�t1Ҁ..36.A
..34.A:�w"�v�.strlen�$��sv�.Malloc��q��..37.A�t2Ҁ..36.A
..37.A:�s�wqv�.memcpy�v�.NewPort�q�qv�.AddObject
..36.A:�rw�q�"��
..41.A:�    �����
..45.A:��`�`�.NewObject�
NewObject �
.NewObject:�`�ut�.strlen�$��rt�.Malloc��q��..43.A�@�"�
..43.A:�r�@qt�.memset�,Bq�vq"�H$�uq"�t�.strcpy�qt�.AddObject�q�"��
..50.A:��`�`�.ReLocate�	ReLocate �
.ReLocate:�`��(..51.A-2)�!��y�.NewPort��@y�.NewMsgBuf��s5�#@�v�y0�@_MyTask�_MyTask�=�$@sy�.InitMCB�z"�sy�.MarshalString�`Osy�.MarshalWord�Asy�.MarshalWord�z"�sy�.MarshalCap�@sy�.IOCMsg��u@����..47.A�&t0z�t1z�@z�uz�w0z�vz�t�z"�H$�s6t4�z"�y�.strcpy�..49.A
..47.A:�vy�.FreePort�uz�
..49.A:�sy�.FreeMsgBuf�u�"��
..51.A:� -1�
..59.A:��`�`�.NewStream�
NewStream �
.NewStream:�`��(..60.A-2)�!��ut�.strlen�&��qt�.Malloc��p��..53.A�@�"�
..53.A:�@p�,Bp�w$�$�p�vp"�H$�t�.NewPort�p�Ap#�t�.InitSemaphore�@p�@p�r0p�up#�t�.strcpy�pt�.AddStream�w$  @$��..55.A�pt�.ReOpen�p5r1$�p�@p4��..55.A�t0�@_StreamLock�_StreamLock�t�.Wait�pt�.Remove�t0�@_StreamLock�_StreamLock�t�.Signal�pt�.Free�@�"�
..55.A:�p�"��
..60.A:� -1����
..64.A:��`�`�.PseudoStream�PseudoStream �
.PseudoStream:�`�@ts�.CheckObject�..62.A�@�"�
..62.A:�t3u$��t"�t"�s�.NewStream��q�"��
..76.A:��`�`�.CopyStream�CopyStream �
.CopyStream:�`��(..77.A-2)�!��!@vu�.CheckStream�..66.A�@�"�
..66.A:�v#�u�.strlen�&��ru�.Malloc��q��..68.A�s0v�@�"�
..68.A:�r�vqu�.memcpy�q7�..70.A�u�.NewPort�q�
..70.A:�v2+..72.A�qvu�.CopyPipe���..72.A�q7u�.FreePort�qu�.Free�s1v�@�"�
..72.A:�q3s2$�q�Aq#�u�.InitSemaphore�qu�.AddStream�q�"��
..77.A:����
������
..95.A:��`�`�.Close�Close �
.Close:�`�@�t�q��..79.A�@�"�
..79.A:�s0�@_Terminating�_Terminating���..81.A�qs�.Abort
..81.A:�	t3!M$�D$��..83.A�"@ts�.CheckStream��p�..85.A�p�"�
..85.A:�t2+..89.A�ts�.PipeClose�
..89.A:�t#�s�.TestWait���..90.A�ts�.Abort�..89.A�
..90.A:�ts�.CloseStream��@t�ts�.Free�..92.A
..83.A:�"@qs�.CheckObject��p�..93.A�p�"�
..93.A:�qs�.CloseObject��@q�qs�.Free
..92.A:�p�"��
..116.A:��`�`�.CloseStream�CloseStream �
.CloseStream:�a�!u0!�!u1!ӄ(..117.A-2)�!�!�@!�!v2,���..99.A�!v3!M$�G$��F��..106.A�`��..102.A�..101.A
..106.A:��`��..99.A�`��..104.A�`��..103.A�..101.A
..102.A:�!v5! %@$�!v;!�.SendException�..99.A
..104.A:�!v5,@$��@�!r�@_MyTask�_MyTask�=�$@!�.InitMCB��!v#��!v#�!�.strlen��///O$���B$�@!�.MarshalWord�`O!�.MarshalWord�A!�.MarshalWord�!v"�!�.MarshalCap�!�.PutMsg�ڀ..99.A
..103.A:�!r�@_Terminating�_Terminating���..99.A�!v;!�.FreePort�..99.A
..101.A:�!q0!�"�
..99.A:�!r�@_Terminating�_Terminating���..114.A�!v7!�.FreePort
..114.A:�!p!�"��
..117.A:��	��
..127.A:��`�`�.CloseObject�CloseObject �
.CloseObject:�a�!t0!�!t1!҄(..128.A-2)�!�!�@�!u3!M$�G$��~�..119.A�~`���..120.A�!u5,@$��@�!q�@_MyTask�_MyTask�=�$@!�.InitMCB��!u"��!u"�!�.strlen��///O$���B$�@!�.MarshalWord�`O!�.MarshalWord�A!�.MarshalWord�!u"�!�.MarshalCap�!�.PutMsg�..119.A
..120.A:�!p0!�"�
..119.A:�!q�@_Terminating�_Terminating���..125.A�!u7!�.FreePort
..125.A:�!�"��
..128.A:��	��
..130.A:��`�`�.Result2�Result2 �
.Result2:�r4"��
..153.A:��`�`�.Abort�Abort �
.Abort:�`��(..154.A-2)�!��v�s0�v3!M$�G$��pE��..140.A�p`��..135.A�p`��..135.A�..133.A
..140.A:��pD��..144.A�p`��..139.A�..133.A
..144.A:��p�..139.A�p`��..139.A�p`��..139.A�..133.A
..135.A:�q2+..150.A�qu�.PipeAbort�..139.A
..150.A:�rq;u�.AbortPort
..139.A:�rq7u�.AbortPort�q7u�.FreePort�@�"�
..133.A:�s1�"��
..154.A:� ��	��
..156.A:��`�`�.AddObject�
AddObject �
.AddObject:�q0�@_ObjectLock�_ObjectLock�q�.Wait�rq0�@_Objects�_Objects�q�.AddTail�q0�@_ObjectLock�_ObjectLock�q�.Signal�"��
..158.A:��`�`�.AddStream�
AddStream �
.AddStream:�q0�@_StreamLock�_StreamLock�q�.Wait�rq0�@_Streams�_Streams�q�.AddTail�q0�@_StreamLock�_StreamLock�q�.Signal�"��
..180.A:��`�`�.CheckObject�CheckObject �
.CheckObject:�`��(..181.A-2)�!��w���..162.A�v0�@_ObjectLock�_ObjectLock�v�.Wait�v0�@_Objects�_Objects�0��
..163.A:�p0�..164.A�pw���..166.A�x"@$��..164.A�wv�.Remove�..164.A�
..166.A:�p0Ѐ..163.A�
..164.A:�v0�@_ObjectLock�_ObjectLock�v�.Signal�pw���..162.A�p2���..162.A�x!@$��..174.A�p2,..174.A�pv�.ReLocate��"�
..174.A:�p7v�.GetPortInfo�..177.A�v�.NewPort�p�
..177.A:�@�"�
..162.A:�t0�"��
..181.A:��	��
..207.A:��`�`�.CheckStream�CheckStream �
.CheckStream:�`��(..208.A-2)�!��x���..185.A�w0�@_StreamLock�_StreamLock�w�.Wait�w0�@_Streams�_Streams�0��
..186.A:�q0�..187.A�qx���..189.A�y"@$��..187.A�xw�.Remove�..187.A�
..189.A:�q0р..186.A�
..187.A:�w0�@_StreamLock�_StreamLock�w�.Signal�qx���..185.A�q2���..185.A�y!@$��..197.A�q2,..197.A�q#�w�.Wait�q2,..200.A�qw�.ReOpen�Ѐ..202.A
..200.A:�@�
..202.A:�q#�w�.Signal�p�"�
..197.A:�y"@$���..203.A�q7w�.GetPortInfo�..203.A�w�.NewPort�q�
..203.A:�@�"�
..185.A:�u0�"��
..208.A:��	��
..210.A:��`�`�.SendIOC�SendIOC �
.SendIOC:�q0�@_MyTask�_MyTask�=r�rq�.PutMsg�"��
..212.A:��`�`�.SendMsg�SendMsg �
.SendMsg:�`��pr�.PutMsg��"��
..214.A:��`�`�.XchMsg1�XchMsg1 �
.XchMsg1:�@rq�.XchMsg�"��
..225.A:��`�`�.IOCMsg�IOCMsg �
.IOCMsg:�`��(..226.A-2)�!��|!L$�y0�|2|�}�..218.A�}|��
..218.A:�@{�._ldtimer��|{�.XchMsg��@{�._ldtimer�w��@x��..224.A�
y1xy2$�����..224.A�sy3$�sy4�y5$�$�Ӏ..218.A
..224.A:�w|�x�"��
..226.A:�@B    �   ����     � �
..235.A:��`�`�.StreamMsg�
StreamMsg �
.StreamMsg:�`��(..236.A-2)�!��|!L$�|2|��
..228.A:�|{�.XchMsg��q@���..229.A�	qy0$�y1���..229.A�}{�.ReOpen��q@����..229.A�};�}7�p|�pԀ..228.A�
..229.A:�q�"��
..236.A:�   �   ��
..238.A:��`�`�._SysNewPort�_SysNewPort �
._SysNewPort:�q�.NewPort�"��
..240.A:��`�`�._SysFreePort�_SysFreePort �
._SysFreePort:�rq�.FreePort�"��
..248.A:��`�`�.BootLink�	BootLink �
.BootLink:�`��(..249.A-2)�!��@�y�.NewPort��z$�$ 0$�$ P�4�0�@z���..243.A�zC��..242.A
..243.A:�w0�"�
..242.A:�@y�.NewMsgBuf��w1�t�v<�@sy�.InitMCB�|�}sy�.MarshalData�@sy�.XchMsg��s2�..246.A�s2y�.FreePort
..246.A:�ty�.FreePort�sy�.FreeMsgBuf�u�"��
..249.A:��	�P `�..dataseg.A 0�_BufLock�_BufLock 3�_IOCBuf�_IOCBuf 2�_ObjectLock�_ObjectLock 3�_Objects�_Objects 3�_StreamLock�_StreamLock 3�_Streams�_Streams 3�_Terminating�_Terminating 1���`�s0�modnum��q�..dataseg.A��t�..251.A�..251.A
..251.A:��"��