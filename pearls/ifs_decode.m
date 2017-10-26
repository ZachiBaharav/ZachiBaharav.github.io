
%%flnCodeIn = 'lena_8_4_80' ;
% --> Good demo!! 
flnCodeIn = 'lena_8_16_80' ;
%fln0 = 'baboon.png';
fln0 = 'barbara.png';



str = sprintf('load %s',flnCodeIn) ;
Orig = double ( imread(fln0) );



load(flnCodeIn) ;
%

dSize = 2*rSize ;       % Domain block size is twice the range

% Make sure proper size
A = Orig(1:R,1:C) ;

figure
figure('Name',flnCodeIn,'NumberTitle','off')
subplot(3,3,1)
imagesc(A)
colormap gray
axis square
title('Starting point')

B = zeros(size(A)) ; % we are transforming from A to B
for iteration = 2:9,

    ii = 0 ; 
    for rr=0:rSize:(R-rSize),
        rdx = rr + (1:rSize);
        fprintf('Now transforming for block=%d (%d)\n',rr/rSize*C/rSize+1, R/rSize*C/rSize )
        for cc=0:rSize:(C-rSize),
            cdx = cc + (1:rSize);
            ii = ii+1 ;
            ind1 = Tindex(ii) ;  
            alpha = Talpha(ii) ;
            beta = Tbeta(ii) ; 
            
            indr = dTopR(ind1)-1 + (1:dSize) ;
            indc = dTopC(ind1)-1 + (1:dSize) ;
            x   = A(indr,indc) ;
             % Spatial shrink by 2
            x = ( x(1:2:end, 1:2:end) + ...
              x(2:2:end, 1:2:end) + ...
              x(1:2:end, 2:2:end) + ...
              x(2:2:end, 2:2:end)  )/4 ;
        
            %xMean = mean(x(:)) ;
            %x0 = x - xMean ;
            

            newB = alpha * x  +  beta ;

            B(rdx,cdx)  = newB ;

        end
    end
    
    A = B ; 
    subplot(3,3,iteration)
    imagesc(A)
    colormap gray
    axis square
    str = sprintf('Iteration %d',iteration);
    title(str)
    
end

