if 1,    
    flnIn = 'lena.png';
    rSize   = 8 ;     % Block side size for Range
    dStep   = 2 ;     % Step size for Domain selection
    alphaMax = 0.8 ;
    A = double ( imread(flnIn) );
end

if 0,
    flnIn = 'self';
    rSize   = 2 ;     % Block side size
    dStep   = 2 ;     %
    alphaMax = 0.8 ;
    A = [ ...
        0 1 0 4
        0 1 0 4
        1 1 0 0 
        0 0 8 8];
end

flnCodeOut = sprintf('%s_%d_%d_%d',flnIn(1:end-4),rSize,dStep,round(100*alphaMax));


dSize = 2*rSize ;       % Domain block size is twice the range

% Make sure proper size
[R,C] = size(A) ;

R = R - rem(R,rSize) ;
C = C - rem(C,rSize) ; 
A =  A(1:R, 1:C) ;


figure
imagesc(A)
colormap gray
axis square


% create the dictionary of blocks
NR = R/rSize * C/rSize ;                        % Number of Range Blocks
ND = floor((R-dSize)/dStep+1) * floor((C-dSize)/dStep+1) ;      % Number of Domain Blocks

xMean = zeros(ND,1) ;
xMat  = zeros(rSize*rSize,ND) ; 
xTxInv   = zeros(ND,1);
% just for convinience
dTopR = zeros(ND,1) ;
dTopC = dTopR ;

ii = 0 ;
for rr=0:dStep:(R-dSize),
    rdx = rr + (1:dSize);
    for cc=0:dStep:(C-dSize),
        cdx = cc + (1:dSize);
        ii = ii+1 ;
        
        x   = A(rdx,cdx);
        
        % Spatial shrink by 2
        x = ( x(1:2:end, 1:2:end) + ...
              x(2:2:end, 1:2:end) + ...
              x(1:2:end, 2:2:end) + ...
              x(2:2:end, 2:2:end)  )/4 ;
        
        x   = x(:) ;
        dTopR(ii) = rdx(1) ;
        dTopC(ii) = cdx(1) ;
        xMean(ii) = mean(x) ;
        x0 = x - xMean(ii) ;
        xMat(:,ii) = x0 ; 
        xTxInv(ii) = 1/(x0'*x0+eps) ; 
    end
end

% For every Range block, look for best fit in the Domain blocks
Tindex = zeros(NR,1) ;
Talpha = Tindex ;
Tbeta = Tindex ;

ii = 0 ;
for rr=0:rSize:(R-rSize),
    rdx = rr + (1:rSize);
    fprintf('Now searching fit for block=%d (%d)\n',rr/rSize*C/rSize+1, R/rSize*C/rSize )
    for cc=0:rSize:(C-rSize),
        cdx = cc + (1:rSize);
        ii = ii+1 ;
        
        y   = A(rdx,cdx) ;
        
        y   = y(:) ;
        yMean = mean(y) ;
        y0 = y - yMean ;
        
        
        xTy = xMat'*y0  ; % just to make it as  one scalar per row.
        
        alpha = xTxInv.*xTy ; 
        alpha = sign(alpha).*min(abs(alpha),alphaMax);
        
        
        % matrix way
        if 0,
            e = (alpha*ones(1,xSize*xSize))'.*xMat - y0*ones(1,N) ;
            e2 = sum(e.*e) ;
            [~,ind] = min(e2) ;
            ind1 = ind(1) ;
        end
        % loop way
        if 1,
           e2 = 0 ;
           for jj=1:ND,
               e = alpha(jj)*xMat(:,jj) - y0 ;
               if (e'*e < e2 || jj==1),
                   e2 = e'*e ;
                   ind1 = jj;
               end
           end
        end

        Tindex(ii) = ind1 ;
        Talpha(ii) = alpha(ind1) ;
        Tbeta(ii) = yMean - alpha(ind1)*xMean(ind1) ;
    end
end


%str = sprintf('save %s Tindex Talpha Tbeta rSize dStep R C dTopR dTopC',flnCodeOut);
%eval(str)
save(flnCodeOut,'Tindex', 'Talpha', 'Tbeta', 'rSize','dStep','R','C','dTopR','dTopC');
