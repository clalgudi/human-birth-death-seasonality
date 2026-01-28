% This script analyze peak timing of flu cases vs. deaths

%% load flu data - WHO
opts = delimitedTextImportOptions("NumVariables", 38);
opts.DataLines = [2, Inf]; opts.Delimiter = ",";
opts.VariableNames = ["WHOREGION", "FLUSEASON", "HEMISPHERE", "ITZ", "COUNTRY_CODE", "COUNTRY_AREA_TERRITORY", "ISO_WEEKSTARTDATE", "ISO_YEAR", "ISO_WEEK", "ISOYW", "AGEGROUP_CODE", "ILI_CASE", "ILI_OUTPATIENTS", "ILI_POP_COV", "SARI_CASE", "SARI_INPATIENTS", "SARI_POP_COV", "SARI_DEATHS", "ARI_CASE", "ARI_OUTPATIENTS", "ARI_POP_COV", "PNEU_CASE", "PNEU_INPATIENTS", "PNEU_POP_COV", "GEOSPREAD", "IMPACT", "INTENSITY", "TREND", "GEOSPREAD_COMMENTS", "COMMENTS", "ILI_NB_SITES", "SARI_NB_SITES", "ARI_NB_SITES", "PNEU_NB_SITES", "MMWR_WEEKSTARTDATE", "MMWR_YEAR", "MMWR_WEEK", "MMWRYW"];
opts.VariableTypes = ["categorical", "categorical", "categorical", "categorical", "categorical", "categorical", "datetime", "double", "double", "double", "categorical", "double", "double", "double", "double", "double", "string", "double", "string", "string", "string", "string", "string", "string", "string", "string", "string", "double", "string", "string", "double", "double", "string", "string", "datetime", "double", "double", "double"];
opts.ExtraColumnsRule = "ignore"; opts.EmptyLineRule = "read";
opts = setvaropts(opts, ["SARI_POP_COV", "ARI_CASE", "ARI_OUTPATIENTS", "ARI_POP_COV", "PNEU_CASE", "PNEU_INPATIENTS", "PNEU_POP_COV", "GEOSPREAD", "IMPACT", "INTENSITY", "GEOSPREAD_COMMENTS", "COMMENTS", "ARI_NB_SITES", "PNEU_NB_SITES"], "WhitespaceRule", "preserve");
opts = setvaropts(opts, ["WHOREGION", "FLUSEASON", "HEMISPHERE", "ITZ", "COUNTRY_CODE", "COUNTRY_AREA_TERRITORY", "AGEGROUP_CODE", "SARI_POP_COV", "ARI_CASE", "ARI_OUTPATIENTS", "ARI_POP_COV", "PNEU_CASE", "PNEU_INPATIENTS", "PNEU_POP_COV", "GEOSPREAD", "IMPACT", "INTENSITY", "GEOSPREAD_COMMENTS", "COMMENTS", "ARI_NB_SITES", "PNEU_NB_SITES"], "EmptyFieldRule", "auto");
opts = setvaropts(opts, "ISO_WEEKSTARTDATE", "InputFormat", "yyyy-MM-dd", "DatetimeFormat", "preserveinput");
opts = setvaropts(opts, "MMWR_WEEKSTARTDATE", "InputFormat", "yyyy-MM-dd", "DatetimeFormat", "preserveinput");
tabflu = readtable("VIW_FID_EPI.csv", opts);

tabflu = renamevars(tabflu, ["COUNTRY_AREA_TERRITORY" "ISO_WEEKSTARTDATE" "ISO_YEAR" "ISO_WEEK" "ILI_CASE"], ["Country" "Date" "Year" "Week" "Count"]);

tabflu = tabflu(ismember(tabflu.AGEGROUP_CODE, ["ALL", "All"]),:); % only keep all age group report
tabflu = tabflu(~isnan(tabflu.Count),:);
tabflu.Country = renamecats(tabflu.Country, "Democratic People's Republic of Korea", "Republic of Korea");
tabflu = tabflu(tabflu.Year<2020,:); % 1996 - 2019 % exclude COVID period
tabflu = sortrows(tabflu, {'Country','Date'}, "ascend");
tabflu(tabflu.Country=="Republic of Moldova"&tabflu.Count>5e3,:)=[];
clear opts idx

load("step1_data.mat", 'countries') % load birth & death data
countries = removevars(countries, ["Birth_avg","Birth_avg_ci" "Death_avg","Death_avg_ci"]);
mm = 2; % death

countries_flu = countries(countries.Death_pvals(:,1)<1e-10,:); 

%% add flu data to countries_flu
Nc = size(countries_flu,1);
countries_flu.flu = cell(Nc,1);
toremove = false(Nc,1);
for nc = 1:Nc
     idx = tabflu.Country==countries_flu.Country(nc);
     if sum(idx)<150 || all(tabflu.Count(idx)==0) % fewer than 150 flu date entries OR all zero entries
         toremove(nc) = true; 
         continue; 
     end 
     countries_flu.flu{nc} = tabflu(idx, ["Country" "Date" "Year" "Week" "Count"]);
end
countries_flu(toremove,:) = []; Nc = size(countries_flu,1);
clear nc toremove idx 
clear tabflu countries

%% Compare peaks of individual countries
for nc = 1:Nc
    figure(10); clf
    yyaxis left; 
    plot(countries_flu.data{nc,mm}.Year+(findgroups(countries_flu.data{nc,mm}.Month)-.5)/12, countries_flu.data{nc,mm}.prctDev*1,'.-')
    ylabel('\Delta_d_e_a_t_h');
    yyaxis right;
    plot(countries_flu.flu{nc}.Year+(days(countries_flu.flu{nc}.Date-datetime(countries_flu.flu{nc}.Year,1,1))+3)/365, countries_flu.flu{nc}.Count,'.-'); 
    ylabel('ILI cases')
    xlabel('Year'); xlim([2008 2020]); xticks(2008:2020); grid on
    title(countries_flu.Country(nc)); pause
end
clear nc

% examples
list = find(ismember(countries_flu.Abbrev, ["Switzerland" "Ireland" "Spain" "Israel"])); %"Denmark" "Netherlands"  "Croatia" 
figure;
for kk = 1:length(list)
    nc = list(kk);
    clf
    yyaxis left; 
    plot(countries_flu.data{nc,mm}.Year+(findgroups(countries_flu.data{nc,mm}.Month)-.5)/12, countries_flu.data{nc,mm}.prctDev*1,'.-')
    ylabel('\Delta_d_e_a_t_h');
    yyaxis right;
    plot(countries_flu.flu{nc}.Year+(days(countries_flu.flu{nc}.Date-datetime(countries_flu.flu{nc}.Year,1,1))+3)/365, countries_flu.flu{nc}.Count,'.-'); 
    ylabel('ILI cases')
    xlabel('Year'); xlim([2008.7 2020]); xticks(2009:2020); grid on
    title(countries_flu.Country(nc)); pause
end
clear nc

%% Calculate death & flu peaks (cycle August to July)
years = 2009:2019; Ny = length(years);
months = categories(countries_flu.data{1,mm}.Month);

peak_death = nan(Nc, Ny);
peak_flu = nan(Nc, Ny);

for nc = 1:Nc % death peaks
    for ny = 1:Ny
        if countries_flu.Latitude(nc)>0
            idx1 = countries_flu.data{nc, mm}.Year == years(ny) & ismember(countries_flu.data{nc, mm}.Month, months(7:end)) & countries_flu.data{nc,mm}.filter==0;
            idx2 = countries_flu.data{nc, mm}.Year == years(ny)+1 & ismember(countries_flu.data{nc, mm}.Month, months(1:6)) & countries_flu.data{nc,mm}.filter==0;
        else
            idx1 = countries_flu.data{nc, mm}.Year == years(ny) & ismember(countries_flu.data{nc, mm}.Month, months(1:6)) & countries_flu.data{nc,mm}.filter==0;
            idx2 = countries_flu.data{nc, mm}.Year == years(ny) & ismember(countries_flu.data{nc, mm}.Month, months(7:12)) & countries_flu.data{nc,mm}.filter==0;
        end
        if sum(idx1|idx2)==12
            peak_death(nc, ny) = calcMeanMonth(1:12, (countries_flu.data{nc, mm}.prctDev(idx1|idx2)/100+1)/12); % phase average
        end
    end
end
peak_death = peak_death-6; 

for nc = 1:Nc % flu peaks
    tab = countries_flu.flu{nc}; tab.Date = tab.Date+days(3);
    for ny = 1:Ny
        if countries_flu.Latitude(nc)>0
            idx1 = tab.Year == years(ny) & tab.Date>=datetime(years(ny),7,1);
            idx2 = tab.Year == years(ny)+1 & tab.Date<datetime(years(ny)+1,7,1);
            ndays = days(tab.Date(idx1|idx2)-datetime(years(ny),7,1));
            weights = tab.Count(idx1|idx2);
        else
            idx1 = tab.Year == years(ny) & tab.Date<datetime(years(ny),7,1);
            idx2 = tab.Year == years(ny) & tab.Date>=datetime(years(ny),7,1);
            ndays = days(tab.Date(idx1|idx2)-datetime(years(ny),1,1));
            weights = tab.Count(idx1|idx2);
        end
        if sum(idx1|idx2)>=30 
            peak_flu(nc, ny) = calcFluMeanDate(ndays, weights);
        end
    end
end
peak_flu = peak_flu-6; 

figure; hold on; plot(peak_flu(:,1),peak_death(:,1),'r.'); plot(peak_flu(:,2:end),peak_death(:,2:end),'.','Color',[.4 .4 .4])
axis equal; xlabel('Flu peak phase'); ylabel('Death peak phase'); legend(["2009" "2010-2019"],'Location','northwest')
[r, p] = corr(peak_death(:),peak_flu(:),'Type','Spearman','Rows','complete'); title([r,p])
xlim([-6 6]); ylim([-6 6]); xticks(-6:1:6); yticks(-6:1:6); grid on

temp1 = peak_flu(:,1)-.5+3; temp2 = peak_death(:,1)-.5+3;
figure; h = polarhistogram( temp1*pi/6,'FaceColor',[1 .5 .5], 'EdgeColor','k', 'binwidth',pi/45,'Normalization','pdf'); rlim([0 2.5])
figure; polarhistogram( temp2*pi/6, 'EdgeColor','k', 'binwidth',pi/45,'Normalization','pdf'); rlim([0 2.5])

temp1 = peak_flu(:,2:end)-.5+3; temp2 = peak_death(:,2:end)-.5+3;
figure; polarhistogram( temp1(:)*pi/6,'FaceColor',[1 .5 .5], 'EdgeColor','k', 'binwidth',pi/45,'Normalization','pdf'); rlim([0 2])
figure; polarhistogram( temp2(:)*pi/6, 'EdgeColor','k', 'binwidth',pi/45,'Normalization','pdf'); rlim([0 2])

clear ny nc idx1 idx2 ndays weights r p