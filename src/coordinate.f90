subroutine coordinate(u,v,ur,vt,nx,ny,nz,nt,tcx,tcy)
implicit none
integer          :: nx,ny,nz,nt,i,j,k,t
real             :: u(nx,ny,nz,nt), v(nx,ny,nz,nt)
real             :: ur(nx,ny,nz,nt), vt(nx,ny,nz,nt)
real             :: tcx(nz,nt),tcy(nz,nt)
real, parameter  :: pi=3.14159265358979323,default=-9.9900000E+08
real             :: xc,yc
real             :: sitam,sita
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
do t=1,nt

do 200 k=1,nz
      do 100 j=1,ny
      do 100 i=1,nx
 
      xc=float(i)-tcx(k,t)
      yc=float(j)-tcy(k,t)
     
      if(xc.ne.0.0) sitam=abs(atan(yc/xc))

      if(xc.eq.0.0.and.yc.ge.0.0) sita=pi/2.0

      if(xc.eq.0.0.and.yc.lt.0.0) sita=pi*1.5

      if(xc.gt.0.0.and.yc.ge.0.0) sita=sitam

      if(xc.lt.0.0.and.yc.ge.0.0) sita=pi-sitam

      if(xc.lt.0.0.and.yc.lt.0.0) sita=pi+sitam

      if(xc.gt.0.0.and.yc.lt.0.0) sita=pi*2.0-sitam
      if((u(i,j,k,t).ne.default).and.(v(i,j,k,t).ne.default)) then
      ur(i,j,k,t)=u(i,j,k,t)*cos(sita)+v(i,j,k,t)*sin(sita)  
      vt(i,j,k,t)=v(i,j,k,t)*cos(sita)-u(i,j,k,t)*sin(sita)
      endif                        
     100 continue
200 continue
enddo
end subroutine coordinate 

