!------------------------------------------------------------!
!   This subroutine is used to partition the symetric field  !
!                  计算方位角平均切向风                      !
!------------------------------------------------------------!
subroutine symmetric(vt,vtb,nx,ny,nz,nt,tcx,tcy,nr)

implicit none
integer         :: nx,ny,nz,nt,nr,i,j,k,t
real            :: vt(nx,ny,nz,nt), vt2(nx,ny), vtb(nr,nz,nt)
real            :: tcx(nz,nt), tcy(nz,nt)
real, parameter :: default=-9.9900000E+08
real            :: pi,sita,sum,total,gx,gy,scint0
integer         :: n,s
do t=1,nt
do 100 k=1,nz
       do j=1,ny
       do i=1,nx
       vt2(i,j)=vt(i,j,k,t)
       enddo
       enddo
!      i=tcx(k,t)
!      j=tcy(k,t)
!       vtb(1,k,t)=vt(i,j,k,t)
       vtb(1,k,t)=0.0

       pi=4.0*atan(1.0)/180.0

       do 20 n=2,nr
       sita=0.0
       sum=0.0
       total=0.0

            do 15 s=1,360
            sita=float(s)*pi
            gx=tcx(k,t)+float(n-1)*cos(sita)
            gy=tcy(k,t)+float(n-1)*sin(sita)

            if(gx.lt.2.0.or.gx.gt.nx-1.or.gy.lt.2.0.or.gy.gt.ny-1) goto 15

            call SCINEX(gx,gy,vt2,scint0,nx,ny)

            if(scint0.ne.default.and.abs(scint0)<10000) then
                 !write(33,*) scint0 
                 sum=sum+scint0
                  total=total+1.0
            endif
            15 continue
                 vtb(n,k,t)=sum/total
                 !write(33,*) sum,total,vtb(n,k),n,k
       20 continue
100 continue
enddo

        return
        end


!-------------------------------------------------------------------
!
      SUBROUTINE SCINEX(GM,GN,SCALA,SCINTO,lq,lp)
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! THIS SUBROUTINE PRODUCES THE VALUE SCINTO OF A SCALAR FIELD AT A POINT
! GM,GN BY INTERPOLATION OR EXTRAPOLATION OF THE FIELD SCALA  (2-DIRECTI
! BESSEL INTERPOLATION FORMULA). MMIN,MMAX AND NMIN,NMAX ARE THE BOUNDAR
! OF THE GRID ARRAY.
!---------------------------
      REAL SCALA(lq,lp)
!     PRINT *,'IN SCINEX GM,GN,SCALA(2,1)',GM,GN,SCALA(2,1)
      MMIN=1
      NMIN=1
      MMAX=lq
      NMAX=lp
      IGM=int(GM)
      JGN=int(GN)
      FM=GM-IGM
      FN=GN-JGN
      IF(FM.LT.1.E-06)FM=0.
      IF(FN.LT.1.E-06)FN=0.
      MS=MMAX-1
      NS=NMAX-1
      MR=MMIN+1
      NR=NMIN+1
      IF(GM.LT.MMAX)GO TO 60
      IF(GN.LT.NMAX)GO TO 20
      E=GM-MMAX
      T1=E*(SCALA(MMAX,NMAX)-SCALA(MS,NMAX))
      E=GN-NMAX
      T2=E*(SCALA(MMAX,NMAX)-SCALA(MMAX,NS))
      SCINTO=SCALA(MMAX,NMAX)+T1+T2
      RETURN
   20 IF(GN.GE.NMIN)GO TO 40
      E=GM-MMAX
      T1=E*(SCALA(MMAX,NMIN)-SCALA(MS,NMIN))
      E=NMIN-GN
      T2=E*(SCALA(MMAX,NMIN)-SCALA(MMAX,NR))
      SCINTO=SCALA(MMAX,NMIN)+T1+T2
      RETURN
   40 P=SCALA(MMAX,JGN)+FN*(SCALA(MMAX,JGN+1)-SCALA(MMAX,JGN))
      H=SCALA(MS,JGN)+FN*(SCALA(MS,JGN+1)-SCALA(MS,JGN))
      E=GM-MMAX
      SCINTO=P+E*(P-H)
      RETURN
   60 IF(GM.GE.MMIN)GO TO 140
      IF(GN.LT.NMAX)GO TO 80
      E=GN-NMAX
      T2=E*(SCALA(MMIN,NMAX)-SCALA(MMIN,NS))
      E=MMIN-GM
      T1=E*(SCALA(MMIN,NMAX)-SCALA(MR,NMAX))
      SCINTO=SCALA(MMIN,NMAX)+T1+T2
      RETURN
   80 IF(GN.GE.NMIN)GO TO 100
      E=NMIN-GN
      T2=E*(SCALA(MMIN,NMIN)-SCALA(MMIN,NR))
      E=MMIN-GM
      T1=E*(SCALA(MMIN,NMIN)-SCALA(MR,NMIN))
      SCINTO=SCALA(MMIN,NMIN)+T1+T2
      RETURN
  100 E=MMIN-GM
      P=SCALA(MMIN,JGN)+FN*(SCALA(MMIN,JGN+1)-SCALA(MMIN,JGN))
      H=SCALA(MR,JGN)+FN*(SCALA(MR,JGN+1)-SCALA(MR,JGN))
      SCINTO=P+E*(P-H)
      RETURN
  120 E=GN-NMAX
      P=SCALA(IGM,NMAX)+FM*(SCALA(IGM+1,NMAX)-SCALA(IGM,NMAX))
      H=SCALA(IGM,NS)+FM*(SCALA(IGM+1,NS)-SCALA(IGM,NS))
      SCINTO=P+E*(P-H)
      RETURN
  140 IF(GN.GE.NMAX)GO TO 120
      IF(GN.GE.NMIN)GO TO 160
      E=NMIN-GN
      P=SCALA(IGM,NMIN)+FM*(SCALA(IGM+1,NMIN)-SCALA(IGM,NMIN))
      H=SCALA(IGM,NR)+FM*(SCALA(IGM+1,NR)-SCALA(IGM,NR))
      SCINTO=P+E*(P-H)
      RETURN
  160 IF(GM.LT.MS.AND.GM.GE.MR.AND.GN.LT.NS.AND.GN.GE.NR)GO TO 180
      P=SCALA(IGM+1,JGN)+FN*(SCALA(IGM+1,JGN+1)-SCALA(IGM+1,JGN))
         H=SCALA(IGM,JGN)+FN*(SCALA(IGM,JGN+1)-SCALA(IGM,JGN))
      SCINTO=H+FM*(P-H)
      RETURN
  180    FQ=0.25*(FM*FM-FM)
      A=SCALA(IGM,JGN-1)+FM*(SCALA(IGM+1,JGN-1)-SCALA(IGM,JGN-1))     &
      +FQ*(SCALA(IGM+2,JGN-1)+SCALA(IGM-1,JGN-1)-SCALA(IGM+1,JGN-1)-  &
      SCALA(IGM,JGN-1))
      B=SCALA(IGM,JGN)+FM*(SCALA(IGM+1,JGN)-SCALA(IGM,JGN))           &
      +FQ*(SCALA(IGM+2,JGN)+SCALA(IGM-1,JGN)-SCALA(IGM+1,JGN)-        &
      SCALA(IGM,JGN))
      C=SCALA(IGM,JGN+1)+FM*(SCALA(IGM+1,JGN+1)-SCALA(IGM,JGN+1))     &
      +FQ*(SCALA(IGM+2,JGN+1)+SCALA(IGM-1,JGN+1)-SCALA(IGM+1,JGN+1)   &
      -SCALA(IGM,JGN+1))
      D=SCALA(IGM,JGN+2)+FM*(SCALA(IGM+1,JGN+2)-SCALA(IGM,JGN+2))     &
      +FQ*(SCALA(IGM+2,JGN+2)+SCALA(IGM-1,JGN+2)-SCALA(IGM+1,JGN+2)   &
      -SCALA(IGM,JGN+2))
      SCINTO=B+FN*(C-B)+0.25*(FN*FN-FN)*(A+D-B-C)
      RETURN
      END
