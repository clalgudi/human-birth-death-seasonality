% This script calculates LS periodogram p-values using the Baluev method
load("step1_data.mat",'info', 'countries'); 
Nc = size(countries, 1); 
vars = info.type; % ["Birth" "Death"]; 

cmap = turbo(100);
pers = [12 6]; 
countries.Birth_pvals = nan(Nc,length(pers)); countries.Death_pvals = nan(Nc,length(pers));
figure; % overlay of periodograms
for nc = 1:Nc
    for mm = 1:2
        if ismember(countries.filter(nc,mm), [0 1])
            dat = countries.data{nc, mm}; dat = dat(dat.filter~=3,:); % remove incomplete years
            if mm==2; dat = dat(dat.Year<2020,:); end
            if size(dat,1)<3*12; continue; end
            xx = dat.prctDev;
            tt = 12*(dat.Year-dat.Year(1))+double(dat.Month);
            [pxx1, ~] = plomb(xx, tt, 1./pers, 'normalized');
            [pxx, fx] = plomb(xx, tt, 1, 'normalized'); 

            subplot(2,1,mm); hold on; plot(1./fx, pxx, '-', 'LineWidth',0.25, 'Color', cmap(max(1, round(abs(countries.Latitude(nc))/64.2*(100-1) ) + 1), :))

            pval = max(fx)*sqrt(4*pi*var(tt))*exp(-pxx1).*sqrt(pxx1); % Baluev method (Eq. 12) in https://doi.org/10.1111/j.1365-2966.2008.12689.x
            pval(pval>1)=1;
            if mm==1
                countries.Birth_pvals(nc,:) = pval;
            elseif mm==2
                countries.Death_pvals(nc,:) = pval;
            end
        end
    end
end
subplot(2,1,1); xlim([2 36]); xticks(0:3:36); xlabel('Period (month)'); ylabel('Power'); title(vars(1)); set(gca, 'tickdir', 'out')
subplot(2,1,2); xlim([2 36]); xticks(0:3:36); xlabel('Period (month)'); ylabel('Power'); title(vars(2)); set(gca, 'tickdir', 'out')
clear mm nc pval tt xx pxx fx pxx1 dat

tab = countries(:,{'Country','Abbrev','Region','MostPopulousCity','Latitude','Longitude','N_years','Birth_pvals','Death_pvals'});
