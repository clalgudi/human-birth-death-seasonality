% This script analyze death seasonality by age groups and underlying cause of death
%% UK - death by age & sex & cause (respiratory vs. non-respiratory)
% load data
opts = delimitedTextImportOptions("NumVariables", 36);
opts.DataLines = [6, Inf]; opts.Delimiter = ",";
opts.VariableNames = ["Year", "Week", "Date_end", "VarName4", "VarName5", "VarName6", "VarName7", "VarName8", "VarName9", "VarName10", "VarName11", "VarName12", "VarName13", "VarName14", "VarName15", "VarName16", "VarName17", "VarName18", "VarName19", "VarName20", "VarName21", "VarName22", "VarName23", "VarName24", "VarName25", "VarName26", "VarName27", "VarName28", "VarName29", "VarName30", "VarName31", "VarName32", "VarName33", "VarName34", "VarName35", "VarName36"];
opts.VariableTypes = ["double", "double", "datetime", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double"];
opts.ExtraColumnsRule = "ignore"; opts.EmptyLineRule = "read";
opts = setvaropts(opts, "Date_end", "InputFormat", "dd-MMM-yy", "DatetimeFormat", "preserveinput");
opts = setvaropts(opts, ["VarName4", "VarName5", "VarName9", "VarName10", "VarName11", "VarName12", "VarName18", "VarName19", "VarName25", "VarName26", "VarName28", "VarName34"], "TrimNonNumeric", true);
opts = setvaropts(opts, ["VarName4", "VarName5", "VarName9", "VarName10", "VarName11", "VarName12", "VarName18", "VarName19", "VarName25", "VarName26", "VarName28", "VarName34"], "ThousandsSeparator", ",");
tab0 = readtable("UK weekly deaths merged.csv", opts);
tab0.Count = tab0{:,4:end};
tab0.Date = tab0.Date_end - days(3);
tab0 = tab0(:,{'Year','Week','Date','Count'});

opts = delimitedTextImportOptions("NumVariables", 36);
opts.DataLines = [2, 5]; opts.Delimiter = ",";
opts.VariableNames = ["Year", "Week", "Date_end", "VarName4", "VarName5", "VarName6", "VarName7", "VarName8", "VarName9", "VarName10", "VarName11", "VarName12", "VarName13", "VarName14", "VarName15", "VarName16", "VarName17", "VarName18", "VarName19", "VarName20", "VarName21", "VarName22", "VarName23", "VarName24", "VarName25", "VarName26", "VarName27", "VarName28", "VarName29", "VarName30", "VarName31", "VarName32", "VarName33", "VarName34", "VarName35", "VarName36"];
opts.VariableTypes = ["string", "string", "string", "string", "string", "string", "string", "string", "string", "string", "string", "string", "string", "string", "string", "string", "string", "string", "string", "string", "string", "string", "string", "string", "string", "string", "string", "string", "string", "string", "string", "string", "string", "string", "string", "string"];
opts.ExtraColumnsRule = "ignore"; opts.EmptyLineRule = "read";
opts = setvaropts(opts, ["Year", "Week", "Date_end", "VarName4", "VarName5", "VarName6", "VarName7", "VarName8", "VarName9", "VarName10", "VarName11", "VarName12", "VarName13", "VarName14", "VarName15", "VarName16", "VarName17", "VarName18", "VarName19", "VarName20", "VarName21", "VarName22", "VarName23", "VarName24", "VarName25", "VarName26", "VarName27", "VarName28", "VarName29", "VarName30", "VarName31", "VarName32", "VarName33", "VarName34", "VarName35", "VarName36"], "WhitespaceRule", "preserve");
opts = setvaropts(opts, ["Year", "Week", "Date_end", "VarName4", "VarName5", "VarName6", "VarName7", "VarName8", "VarName9", "VarName10", "VarName11", "VarName12", "VarName13", "VarName14", "VarName15", "VarName16", "VarName17", "VarName18", "VarName19", "VarName20", "VarName21", "VarName22", "VarName23", "VarName24", "VarName25", "VarName26", "VarName27", "VarName28", "VarName29", "VarName30", "VarName31", "VarName32", "VarName33", "VarName34", "VarName35", "VarName36"], "EmptyFieldRule", "auto");
vars0 = readmatrix("UK weekly deaths merged.csv", opts)';
vars = table;
for kk = 1:4
    vars.(vars0(1,kk)) = categorical(vars0(4:end, kk));
end

tab = [];
for kk = 1:size(vars,1)
    temp = tab0(:,{'Year','Week','Date'});
    temp.Count = tab0.Count(:,kk);
    temp = [temp repmat(vars(kk,:),size(temp,1),1)];
    tab = [tab; temp];
end
ages = categories(tab.Age); years = unique(tab.Year);

idx = tab.Age=='All' & tab.Sex=='All' & tab.Region=='All';
idx1 = idx & tab.Cause=='All'; idx2 = idx & tab.Cause=='Respiratory diseases';
if all(tab.Year(idx1)==tab.Year(idx2) & tab.Week(idx1)==tab.Week(idx2))
    tab.Count(idx1) = tab.Count(idx1)-tab.Count(idx2);
    tab.Cause(idx1) = "Other causes";
end
vars.Cause(vars.Age=='All'&vars.Sex=='All'&vars.Region=='All'&vars.Cause=='All')='Other causes';
% resolving NANs of several male entries (no missing in All or Female)
idx = tab.Cause=='All' & tab.Region=='All';
idx1 = idx & tab.Sex=='All'; idx2 = idx & tab.Sex=='Female'; idx3 = idx & tab.Sex=='Male';
if all(tab.Year(idx1)==tab.Year(idx2) & tab.Week(idx1)==tab.Week(idx2) & tab.Age(idx1)==tab.Age(idx2) &...
        tab.Year(idx1)==tab.Year(idx3) & tab.Week(idx1)==tab.Week(idx3) & tab.Age(idx1)==tab.Age(idx3) )
    if sum(tab.Count(idx3)+tab.Count(idx2)-tab.Count(idx1),"omitmissing")==0
        tab.Count(idx3) = tab.Count(idx1) - tab.Count(idx2);
    end
end
clear opts vars0 kk temp tab0 idx idx1 idx2 idx3

% preprocessing & normalization
tab.prctDev = nan(size(tab,1),1);
for nv = 1:size(vars,1)
    idx = ismember(tab(:,{'Age','Sex','Cause','Region'}), vars(nv,:));
    for nc = 1:length(years)
        idxx = tab.Year==years(nc) & idx;
        if sum(idxx)>=52
            tab.prctDev(idxx) = 100*(tab.Count(idxx)/sum(tab.Count(idxx))*sum(idxx) - 1);
        end
    end
end
clear na nc nv idx idxx

% plot count
colors = flipud(parula(length(ages)));
figure; hold on % by age
temp = datetime(years,ones(length(years),1),ones(length(years),1));
plot([temp temp], [0 1]*2e4,'color',[1 1 1]*.75, 'HandleVisibility','off');
for na = 1:(length(ages)-1)
    idx = tab.Age==ages(na) & tab.Region=='All' & tab.Cause=='All' & tab.Sex=='All';
    plot(tab.Date(idx), tab.Count(idx),'.', 'Color', colors(na,:));
end
ylabel("Count (deaths)"); xlabel('Year'); legend(ages,'location','eastoutside'); set(gca, 'TickLength',[0.003 0.006], 'TickDir', 'out')
temp = datetime(years,7*ones(length(years),1),ones(length(years),1));
xticks(temp(1:2:end)); xticklabels(years(1:2:end)); ylim([0 7500]); yticks(0:25e3:7500); xlim([datetime(2010,1,1), datetime(2019,12,30)])
title("All by age")

figure; hold on % Female by age
temp = datetime(years,ones(length(years),1),ones(length(years),1));
plot([temp temp], [0 1]*2e4,'color',[1 1 1]*.75, 'HandleVisibility','off');
for na = 1:length(ages)
    idx = tab.Age==ages(na) & tab.Region=='All' & tab.Cause=='All' & tab.Sex=='Female';
    plot(tab.Date(idx), tab.Count(idx),'.', 'Color', colors(na,:));
end
ylabel("Count (deaths)"); xlabel('Year'); legend(ages,'location','eastoutside'); set(gca, 'TickLength',[0.003 0.006], 'TickDir', 'out')
temp = datetime(years,7*ones(length(years),1),ones(length(years),1));
xticks(temp(1:2:end)); xticklabels(years(1:2:end)); ylim([0 5000]); yticks(0:1e3:7500); xlim([datetime(2010,1,1), datetime(2019,12,30)])
title("Female by age")

figure; hold on % Male by age
temp = datetime(years,ones(length(years),1),ones(length(years),1));
plot([temp temp], [0 1]*2e4,'color',[1 1 1]*.75, 'HandleVisibility','off');
for na = 1:length(ages)
    idx = tab.Age==ages(na) & tab.Region=='All' & tab.Cause=='All' & tab.Sex=='Male';
    plot(tab.Date(idx), tab.Count(idx),'.', 'Color', colors(na,:));
end
ylabel("Count (deaths)"); xlabel('Year'); legend(ages,'location','eastoutside'); set(gca, 'TickLength',[0.003 0.006], 'TickDir', 'out')
temp = datetime(years,7*ones(length(years),1),ones(length(years),1));
xticks(temp(1:2:end)); xticklabels(years(1:2:end)); ylim([0 5000]); yticks(0:2e3:7500); xlim([datetime(2010,1,1), datetime(2019,12,30)])
title("Male by age")

figure; hold on % by cause
temp = datetime(years,ones(length(years),1),ones(length(years),1));
plot([temp temp], [0 1]*2e4,'color',[1 1 1]*.75, 'HandleVisibility','off');
idx = tab.Age=='All' & tab.Region=='All' & tab.Cause=='Respiratory diseases' & tab.Sex=='All';
plot(tab.Date(idx), tab.Count(idx),'.', 'Color', 'k');
idx = tab.Age=='All' & tab.Region=='All' & tab.Cause=='Other causes' & tab.Sex=='All';
plot(tab.Date(idx), tab.Count(idx),'.', 'Color', 'b');
ylabel("Count (deaths)"); xlabel('Year'); legend({'Respiratory diseases','Other causes'},'location','eastoutside'); set(gca, 'TickLength',[0.003 0.006], 'TickDir', 'out')
temp = datetime(years,7*ones(length(years),1),ones(length(years),1));
xticks(temp(1:2:end)); xticklabels(years(1:2:end)); ylim([0 1.3e4]); yticks(0:2.5e3:1.3e4); xlim([datetime(2010,1,1), datetime(2019,12,30)])
title("By cause")

clear idx na temp colors

% plot deviation
sexes = categories(tab.Sex);
figure; hold on
count=0;
for ns = 1:length(sexes)
    for na = 1:length(ages)
        idx = tab.Age==ages(na) & tab.Sex==sexes(ns) & tab.Year~=2025 & tab.Region=='All' & tab.Cause=='All' & ~isnan(tab.prctDev);
        if sum(idx)==0; count = count+1; continue; end
        xx = tab.prctDev(idx); tt = days(tab.Date(idx)-datetime(tab.Year(idx), ones(sum(idx),1), ones(sum(idx),1)));
        subplot(4, 6, (ns-1)*length(ages)+na-count); hold on
        plot([.5 732], [0 0], 'k-'); plot([1 1]*15, [-1 1]*50, 'k-'); plot([1 1]*366+15, [-1 1]*50, 'k-');
        plot(tt, xx, '.', 'MarkerSize',1, 'Color', 'b'); plot(tt+365, xx, '.', 'MarkerSize',3, 'Color', 'b')
        set(gca, 'TickLength',[0.003 0.006], 'TickDir', 'out', 'FontSize',8); xlim([0 732]); xticks(0:365:732); xticklabels(''); ylim([-40 50]); yticks(-40:20:50)

        % peridogram
        tt = days(tab.Date(idx)-datetime(2010,1,1));
        [pxx1, ~] = plomb(xx, tt, [1/365 1/182], 'normalized');
        [pxx, fx] = plomb(xx, tt, 1/7, 'normalized');
        pval = max(fx)*sqrt(4*pi*var(tt))*exp(-pxx1(1)).*sqrt(pxx1(1)); % Baluev method (Eq. 12) in https://doi.org/10.1111/j.1365-2966.2008.12689.x
        pval(pval>1)=1;
        if pval>0.001
            pval = num2str(pval,'%.3f');
        else
            pval = num2str(pval,'%.0e');
        end
        title([sexes{ns}+", "+ages{na} "p="+pval]); 
    end
end

% add cause
idx0 = tab.Age=='All' & tab.Sex=='All' & tab.Year~=2025 & tab.Region=='All' & ~isnan(tab.prctDev);
for kk = 1:2
    if kk==1
        idx = idx0 & tab.Cause=='Respiratory diseases';
    else
        idx = idx0 & tab.Cause=='Other causes';
    end
    xx = tab.prctDev(idx); tt = days(tab.Date(idx)-datetime(tab.Year(idx), ones(sum(idx),1), ones(sum(idx),1)));
    subplot(4, 6, kk+length(sexes)*length(ages)-count); hold on 
    plot([.5 732], [0 0], 'k-'); plot([1 1]*15, [-1 1]*50, 'k-'); plot([1 1]*366+15, [-1 1]*50, 'k-');
    plot(tt, xx, '.', 'MarkerSize',1, 'Color', 'b'); plot(tt+365, xx, '.', 'MarkerSize',3, 'Color', 'b')
    set(gca, 'TickLength',[0.003 0.006], 'TickDir', 'out', 'FontSize',8); xlim([0 732]); xticks(0:365:732); xticklabels(''); ylim([-40 50]); yticks(-40:20:50)
    % peridogram
    tt = days(tab.Date(idx)-datetime(2010,1,1));
    [pxx1, ~] = plomb(xx, tt, [1/365 1/182], 'normalized');
    [pxx, fx] = plomb(xx, tt, 1/7, 'normalized');
    pval = max(fx)*sqrt(4*pi*var(tt))*exp(-pxx1(1)).*sqrt(pxx1(1)); % Baluev method (Eq. 12) in https://doi.org/10.1111/j.1365-2966.2008.12689.x
    pval(pval>1)=1;
    if pval>0.001
        pval = num2str(pval,'%.3f');
    else
        pval = num2str(pval,'%.0e');
    end
    if kk==1
        title(["Respiratory diseases" "p="+pval]);
    else
        title(["Other causes" "p="+pval]);
    end
end

% add region
figure;
idx0 = tab.Age=='All' & tab.Sex=='All' & tab.Year~=2025 & tab.Cause=='All' & ~isnan(tab.prctDev);
regions = categories(tab.Region); regions = regions(regions~="All");
for kk = 1:length(regions)
    idx = idx0 & tab.Region==regions(kk);
    if sum(idx)==0; continue; end
    xx = tab.prctDev(idx); tt = days(tab.Date(idx)-datetime(tab.Year(idx), ones(sum(idx),1), ones(sum(idx),1)));
    subplot(4, 6, kk); hold on 
    plot([.5 732], [0 0], 'k-'); plot([1 1]*15, [-1 1]*50, 'k-'); plot([1 1]*366+15, [-1 1]*50, 'k-');
    plot(tt, xx, '.', 'MarkerSize',1, 'Color', 'b'); plot(tt+365, xx, '.', 'MarkerSize',3, 'Color', 'b')
    set(gca, 'TickLength',[0.003 0.006], 'TickDir', 'out', 'FontSize',8); xlim([0 732]); xticks(0:365:732); xticklabels(''); ylim([-40 50]); yticks(-40:20:50)
    % peridogram
    tt = days(tab.Date(idx)-datetime(2010,1,1));
    [pxx1, ~] = plomb(xx, tt, [1/365 1/182], 'normalized');
    [pxx, fx] = plomb(xx, tt, 1/7, 'normalized');
    pval = max(fx)*sqrt(4*pi*var(tt))*exp(-pxx1(1)).*sqrt(pxx1(1)); % Baluev method (Eq. 12) in https://doi.org/10.1111/j.1365-2966.2008.12689.x
    pval(pval>1)=1;
    if pval>0.001
        pval = num2str(pval,'%.3f');
    else
        pval = num2str(pval,'%.0e');
    end
    title([regions(kk) "p="+pval]);
end
clear idx idx0 xx tt fx pxx pxx1 pval kk ns na

% plot  fraction of deaths by age & cause
frac = nan(length(years), length(ages));
for ny = 1:length(years)
    val = sum(tab.Count(tab.Year==years(ny) & tab.Sex=='All' & tab.Region=='All' & tab.Cause=='All'));
    for na = 1:length(ages)
        frac(ny,na) = sum(tab.Count(tab.Year==years(ny) & tab.Age==ages(na)  & tab.Sex=='All' & tab.Region=='All' & tab.Cause=='All'))/val;
    end
end
sfrac = cumsum(frac, 2);
colors = flipud(parula(length(ages)-1));
figure; hold on
for na = 1:length(ages)-1
    if na==1
        fill([years; years(end:-1:1)],[zeros(length(years),1); sfrac(end:-1:1,na)], colors(na,:),'EdgeColor','none') 
    else
        fill([years; years(end:-1:1)],[sfrac(:,na-1); sfrac(end:-1:1,na)], colors(na,:),'EdgeColor','none') % 'FaceColor',colors(ny,:)
    end
end
legend(ages(1:end-1), 'location','eastoutside'); set(gca, 'TickLength',[0.003 0.006], 'TickDir', 'out')

frac = nan(length(years), 2);
for ny = 1:length(years)
    val = sum(tab.Count(tab.Year==years(ny) & tab.Sex=='All' & tab.Age=='All' & tab.Region=='All'));
    frac(ny,1) = sum(tab.Count(tab.Year==years(ny) & tab.Cause=='Respiratory diseases' & tab.Sex=='All'& tab.Age=='All' & tab.Region=='All'))/val;
    frac(ny,2) = sum(tab.Count(tab.Year==years(ny) & tab.Cause=='Other causes' & tab.Sex=='All'& tab.Age=='All' & tab.Region=='All'))/val;    
end
sfrac = cumsum(frac, 2);

figure; hold on
fill([years; years(end:-1:1)],[zeros(length(years),1); sfrac(end:-1:1,1)], 'b','EdgeColor','none')
fill([years; years(end:-1:1)],[sfrac(:,1); sfrac(end:-1:1,2)], 'k','EdgeColor','none') 
legend({'Respiratory diseases' 'Other causes'}, 'location','eastoutside'); set(gca, 'TickLength',[0.003 0.006], 'TickDir', 'out')
clear frac sfrac na ny val colors

%% New Zealand - death by age & sex

% load data
opts = spreadsheetImportOptions("NumVariables", 6);
opts.Sheet = "Data"; opts.DataRange = "A2:F29758";
opts.VariableNames = ["Year", "Month", "Ethnicity", "Sex", "Age", "Count"];
opts.VariableTypes = ["double", "double", "categorical", "categorical", "categorical", "double"];
opts = setvaropts(opts, ["Ethnicity", "Sex", "Age"], "EmptyFieldRule", "auto");
tab0 = readtable("New Zeland monthly-death-registrations-by-ethnicity-age-and-sex-January-2010-March-2025.xlsx", opts, "UseExcel", false);
tab0 = tab0(tab0.Year<=2019,:); % exclude COVID period
years = unique(tab0.Year); months = unique(tab0.Month);
clear opts

% analyze by age bins
ages0 = categories(tab0.Age);
% ---------age bin every 15 years
ages = {'0' '1-14' '15-44' '45-64' '65-74' '75-84' '85+'}; MM = 3;

tab = [];
for ny = 1:length(years)
    for nm = 1:length(months)
        for na = 1:length(ages)
            temp = table; temp.Year = years(ny); temp.Month = months(nm); temp.Age = ages(na);
            idx = tab0.Year==years(ny) & tab0.Month==months(nm);
            if sum(idx)==0; continue; end
            if na==1
                idx = idx & tab0.Age=='00_00';
            else
                loc = (na-2)*MM+(1:MM); 
                if loc(2)>length(ages0); loc(2) = length(ages0); end
                idx = idx & ismember(tab0.Age, ages0(loc));
            end
            temp.Sex = "All";
            temp.Count = sum(tab0.Count(idx));
            tab = [tab; temp];
            temp.Sex = "Female";
            temp.Count = sum(tab0.Count(idx & tab0.Sex=="Female"));
            tab = [tab; temp];
            temp.Sex = "Male";
            temp.Count = sum(tab0.Count(idx & tab0.Sex=="Male"));
            tab = [tab; temp];
        end
    end
end
tab.Age = categorical(tab.Age, ages);
tab.Sex = categorical(tab.Sex, {'All','Female','Male'});

ages = categories(tab.Age); sexes = categories(tab.Sex);
% preprocessing & normalization
nDays = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31; ... % n days per month from Jan to Dec (normal year)
    31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]; % n days per month from Jan to Dec (leap year)
tab.prctDev = nan(size(tab,1),1);
for na = 1:length(ages)
    for ns = 1:length(sexes)
        for ny = 1:length(years)
            idx = tab.Age==ages(na) & tab.Sex==sexes(ns) & tab.Year==years(ny);
            if sum(idx)==12
                if mod(years(ny), 4)==0 % leap year
                    ndays = nDays(2,:)';
                else %normal year
                    ndays = nDays(1,:)';
                end
                tab.prctDev(idx) = 100*(tab.Count(idx)/sum(tab.Count(idx))./ndays*sum(ndays) - 1);
            end
        end
    end
end
clear idx na ns ny temp nm ages0 loc

% ========== plot count by all age group & sex =============
colors = flipud(parula(length(ages)));
figure;
for ns = 1:3
    subplot(3,1,ns); hold on % All
    plot([years years]+7/12, [0 5000],'color',[1 1 1]*.75, 'HandleVisibility','off');
    for na = 1:length(ages)
        idx = tab.Age==ages(na) & tab.Sex==sexes(ns);
        plot(tab.Year(idx)+(tab.Month(idx)-1)/12, tab.Count(idx),'.', 'Color',colors(na,:));
    end
    ylabel("Count (deaths)"); xlabel('Year'); legend(ages,'location','eastoutside'); set(gca, 'TickLength',[0.003 0.006], 'TickDir', 'out')
    xticks(years(1:2:end)); xticklabels(years(1:2:end)); xlim([2010 max(years)+1]); title(sexes(ns))
    if ns==1
        ylim([0 1010*MM]);
    else
        ylim([0 505*MM]);
    end
end
clear na idx ns colors
 
% ========== plot deviation ===========
figure; hold on 
for ns = 1:length(sexes)
    for na = 1:length(ages)
        idx = tab.Age==ages(na) & tab.Sex==sexes(ns) & tab.Year~=2025;
        xx = reshape(tab.prctDev(idx), [12 sum(idx)/12]); 
        xx = xx([7:12 1:6],:); % shifted to align season
        subplot(5, 6, (ns-1)*length(ages)+na); hold on
        plot([.5 24.5], [0 0], 'k-'); plot([1 1], [-1 1]*50, 'k-'); plot([13 13], [-1 1]*50, 'k-'); 
        scatter(1:24 , [xx' xx'], 10, 'b', 'filled','MarkerEdgeColor','none','MarkerFaceAlpha',.5); 
        set(gca, 'TickLength',[0.003 0.006], 'TickDir', 'out', 'FontSize',8); xlim([0 24]); xticks(3:3:24); xticklabels(''); ylim([-40 50]); yticks(-40:20:50)

        % peridogram
        xx = xx(:); tt = tab.Year(idx)+(tab.Month(idx)-1)/12;
        [pxx1, ~] = plomb(xx, tt, [1 2], 'normalized');
        [pxx, fx] = plomb(xx, tt, 12, 'normalized');
        pval = max(fx)*sqrt(4*pi*var(tt))*exp(-pxx1(1)).*sqrt(pxx1(1)); % Baluev method (Eq. 12) in https://doi.org/10.1111/j.1365-2966.2008.12689.x
        pval(pval>1)=1;
        if pval>0.001
            pval = num2str(pval,'%.3f');
        else
            pval = num2str(pval,'%.0e');
        end
        title([sexes{ns}+", "+ages{na} "p="+pval]); 
    end
end
clear idx xx tt avg ci pxx1 pxx pval fx na ns ny 

% plot  fraction of deaths by age (year resolution)
frac = nan(length(years), length(ages));
for ny = 1:length(years)
    val = sum(tab.Count(tab.Year==years(ny) & tab.Sex=='All'));
    for na = 1:length(ages)
        frac(ny,na) = sum(tab.Count(tab.Year==years(ny) & tab.Age==ages(na)  & tab.Sex=='All'))/val;
    end
end
sfrac = cumsum(frac, 2);
colors = flipud(parula(length(ages)));
figure; hold on
for na = 1:length(ages)
    if na==1
        fill([years; years(end:-1:1)],[zeros(length(years),1); sfrac(end:-1:1,na)], colors(na,:),'EdgeColor','none') %'FaceColor',colors(ny,:),
    else
        fill([years; years(end:-1:1)],[sfrac(:,na-1); sfrac(end:-1:1,na)], colors(na,:),'EdgeColor','none') % 'FaceColor',colors(ny,:)
    end
end
legend(ages, 'location','eastoutside'); set(gca, 'TickLength',[0.003 0.006], 'TickDir', 'out')
clear frac sfrac na ny val colors

% plot  fraction of deaths by age (same resolution as seasonality)
tab1 = tab(tab.Sex=='All',:);
tab1.fract = nan(size(tab1,1),1);
for ny = 1:length(years)
    for nm = 1:12
        idx = tab1.Year==years(ny) & tab1.Month==nm;
        tab1.fract(idx) = tab1.Count(idx)/sum(tab1.Count(idx));
    end
end
sfrac = reshape(tab1.fract, [length(ages) length(years)*12])';
sfrac = cumsum(sfrac, 2);
colors = flipud(parula(length(ages)));
figure; hold on
for na = 1:length(ages)
    if na==1
        fill([tab1.Year(1:length(ages):end)+tab1.Month(1:length(ages):end)/12; tab1.Year(end:-length(ages):1)+tab1.Month(end:-length(ages):1)/12],...
            [zeros(size(tab1,1)/length(ages),1); sfrac(end:-1:1,na)], colors(na,:),'EdgeColor','none') %'FaceColor',colors(ny,:),
    else
        fill([tab1.Year(1:length(ages):end)+tab1.Month(1:length(ages):end)/12; tab1.Year(end:-length(ages):1)+tab1.Month(end:-length(ages):1)/12],...
            [sfrac(:,na-1); sfrac(end:-1:1,na)], colors(na,:),'EdgeColor','none') % 'FaceColor',colors(ny,:)
    end
end
legend(ages, 'location','eastoutside'); set(gca, 'TickLength',[0.003 0.006], 'TickDir', 'out')
clear frac sfrac na ny val colors

%% Netherlands - deaths by age

% load date
opts = delimitedTextImportOptions("NumVariables", 4, "Encoding", "UTF-8");
opts.DataLines = [2, Inf]; opts.Delimiter = ";";
opts.VariableNames = ["Sex", "Age", "Time", "Count"];
opts.VariableTypes = ["categorical", "categorical", "char", "double"];
opts.ExtraColumnsRule = "ignore"; opts.EmptyLineRule = "read";
opts = setvaropts(opts, "Time", "WhitespaceRule", "preserve");
opts = setvaropts(opts, ["Sex", "Age", "Time"], "EmptyFieldRule", "auto");
tab0 = readtable("Netherlands Deaths__weekly_23062025_083944.csv", opts);

for kk = 1:size(tab0,1)
    temp = tab0.Time{kk};
    if temp(1) == ' '
        tab0.Time{kk} = temp(2:end);
    end
end

tab0.Sex = reordercats(renamecats(tab0.Sex, 'Total male and female', 'All'), {'All','Female','Male'});
tab0.Age = renamecats(tab0.Age, {'0 to 64 years' '65 to 79 years' '80 years or older' 'Total'}, {'0-64' '65-79' '80+' 'All'});
temp = char(tab0.Time);
tab0.Year = str2double(string(temp(:,1:4))); 
tab0.Week = str2double(string(temp(:,11:12)));
tab0 = tab0(~isnan(tab0.Week),:);

tab0.Year(tab0.Year==1080) = 1980; % correct obvious typo
idx0 = find(tab0.Year(2:end-1)~=tab0.Year(1:end-2) & tab0.Year(2:end-1)~=tab0.Year(3:end));
idx = find(~ismember(tab0.Week(idx0+1), [0 1]));
idx = idx0(idx)+1;
if all(tab0.Year(idx-1)==tab0.Year(idx+1))
    tab0.Year(idx) = tab0.Year(idx-1);
end
idx0 = find(tab0.Week(2:end-1)~=tab0.Week(1:end-2)+1 & tab0.Week(2:end-1)~=tab0.Week(3:end)-1);
idx = find(~ismember(tab0.Week(idx0+1), [0 1]));
idx = idx0(idx)+1;
if all(tab0.Week(idx-1)+2==tab0.Week(idx+1))
    tab0.Week(idx) = tab0.Week(idx-1)+1;
end

temp = char(tab0.Time);
tab0.Day = str2double(string(temp(:,14)));
tab0.Day(tab0.Week==1) = tab0.Day(tab0.Week==1)-7;
tab0.Countr = tab0.Count;
idx = find(tab0.Week==0);
tab0.Countr(idx(2:end)-1) = tab0.Count(idx(2:end)-1)+tab0.Count(idx(2:end));
idx = find(tab0.Week==53);
tab0.Countr(idx+1) = tab0.Count(idx+1)+tab0.Count(idx);

tab0.Dayr = 7*(tab0.Week-1)+4; 
years = unique(tab0.Year);
for ny = 1:(length(years)-1)
    idx = tab0.Year==years(ny); 
    val = tab0.Day(idx&tab0.Week==0); val = val(~isnan(val)); val = unique(val);
    if ~isempty(val)
        if isscalar(val)
            tab0.Dayr(idx) = tab0.Dayr(idx) + val;
        else
            disp(years(ny))
        end
    end
    val = tab0.Day(idx&tab0.Week==1); val = val(~isnan(val)); val = unique(val);
    if ~isempty(val)
        if isscalar(val)
            tab0.Dayr(idx) = tab0.Dayr(idx) + val;
        else
            disp(years(ny))
        end
    end
end

tab0 = tab0(tab0.Dayr>0 & tab0.Dayr<=366,:);
tab0 = tab0(~(tab0.Dayr==366 & mod(tab0.Year,4)~=0),:);
tab = tab0(:,{'Year','Week','Dayr','Sex','Age','Countr',});
tab.Properties.VariableNames{strcmp(tab.Properties.VariableNames,'Dayr')} = 'Day';
tab.Properties.VariableNames{strcmp(tab.Properties.VariableNames,'Countr')} = 'Count';
vars = unique(tab(:,{'Sex','Age'}));


clear opts kk idx ny val temp

% preprocessing & normalization
tab.prctDev = nan(size(tab,1),1);
for nv = 1:size(vars,1)
    idx = ismember(tab(:,{'Age','Sex'}), vars(nv,:));
    for nc = 1:length(years)
        idxx = tab.Year==years(nc) & idx;
        tab.prctDev(idxx) = 100*(tab.Count(idxx)/sum(tab.Count(idxx))*sum(idxx) - 1);
    end
end
clear na nc nv idx idxx

% plot count
ages = categories(tab.Age);
colors = flipud(parula(length(ages)));
% by age
figure; hold on
plot([years years], [0 1]*3e3,'color',[1 1 1]*.75, 'HandleVisibility','off');
for na = 1:(length(ages)-1)
    idx = tab.Age==ages(na) & tab.Sex=='All';
    plot(tab.Year(idx)+tab.Day(idx)/365, tab.Count(idx),'.', 'Color', colors(na,:));
end
ylabel("Count (deaths)"); xlabel('Year'); legend(ages,'location','eastoutside'); set(gca, 'TickLength',[0.003 0.006], 'TickDir', 'out')
xticks(years(1:2:end)); xticklabels(years(1:2:end)); ylim([0 3e3]); yticks(0:500:3e3); xlim([years(1) years(end)+1])
title("All by age")

% Female by age
figure; hold on
plot([years years], [0 1]*3e3,'color',[1 1 1]*.75, 'HandleVisibility','off');
for na = 1:(length(ages)-1)
    idx = tab.Age==ages(na) & tab.Sex=='Female';
    plot(tab.Year(idx)+tab.Day(idx)/365, tab.Count(idx),'.', 'Color', colors(na,:));
end
ylabel("Count (deaths)"); xlabel('Year'); legend(ages,'location','eastoutside'); set(gca, 'TickLength',[0.003 0.006], 'TickDir', 'out')
xticks(years(1:2:end)); xticklabels(years(1:2:end)); ylim([0 1.5e3]); yticks(0:500:1.5e3); xlim([years(1) years(end)+1])
title("Female by age")

% Male by age
figure; hold on
plot([years years], [0 1]*3e3,'color',[1 1 1]*.75, 'HandleVisibility','off');
for na = 1:(length(ages)-1)
    idx = tab.Age==ages(na) & tab.Sex=='Male';
    plot(tab.Year(idx)+tab.Day(idx)/365, tab.Count(idx),'.', 'Color', colors(na,:));
end
ylabel("Count (deaths)"); xlabel('Year'); legend(ages,'location','eastoutside'); set(gca, 'TickLength',[0.003 0.006], 'TickDir', 'out')
xticks(years(1:2:end)); xticklabels(years(1:2:end)); ylim([0 1.5e3]); yticks(0:500:1.5e3); xlim([years(1) years(end)+1])
title("Male by age")

clear idx na temp colors

% plot deviation
sexes = categories(tab.Sex);
figure; hold on
count=0;
for ns = 1:length(sexes)
    for na = 1:length(ages)
        idx = tab.Age==ages(na) & tab.Sex==sexes(ns) & tab.Year~=2025 & ~isnan(tab.prctDev);
        if sum(idx)==0; count = count+1; continue; end
        xx = tab.prctDev(idx); tt = tab.Day(idx)/365;
        % subplot(length(sexes), length(ages), (ns-1)*length(ages)+na); hold on
        subplot(5, 6, (ns-1)*length(ages)+na-count); hold on
        plot([.5 732], [0 0], 'k-'); plot([1 1]*15, [-1 1]*50, 'k-'); plot([1 1]*366+15, [-1 1]*50, 'k-');
        % plot([tt; tt+1], [xx; xx], '.', 'MarkerSize',1, 'Color', 'b');
        scatter([tt; tt+1], [xx; xx], 3, 'b','filled','markeredgecolor','none','markerfacealpha',.2); 
        set(gca, 'TickLength',[0.003 0.006], 'TickDir', 'out', 'FontSize',8); xlim([0 2]); xticks(0:1:2); xticklabels(''); ylim([-30 50]); yticks(-40:20:50)

        % peridogram
        tt = tab.Year(idx)+tab.Day(idx)/365;
        [pxx1, ~] = plomb(xx, tt, [1 2], 'normalized');
        [pxx, fx] = plomb(xx, tt, 1/53, 'normalized');
        pval = max(fx)*sqrt(4*pi*var(tt))*exp(-pxx1(1)).*sqrt(pxx1(1)); % Baluev method (Eq. 12) in https://doi.org/10.1111/j.1365-2966.2008.12689.x
        pval(pval>1)=1;
        if pval>0.001
            pval = num2str(pval,'%.3f');
        else
            pval = num2str(pval,'%.0e');
        end
        title([sexes{ns}+", "+ages{na} "p="+pval]); 
    end
end
clear idx idx0 fx pxx pxx1 pval tt xx na ns count

% plot  fraction of deaths by age
frac = nan(length(years), length(ages));
for ny = 1:length(years)
    val = sum(tab.Count(tab.Year==years(ny) & tab.Sex=='All'));
    for na = 1:length(ages)
        frac(ny,na) = sum(tab.Count(tab.Year==years(ny) & tab.Age==ages(na)  & tab.Sex=='All'))/val;
    end
end
sfrac = cumsum(frac, 2);
colors = flipud(parula(length(ages)));
figure; hold on
for na = 1:length(ages)
    if na==1
        fill([years; years(end:-1:1)],[zeros(length(years),1); sfrac(end:-1:1,na)], colors(na,:),'EdgeColor','none') 
    else
        fill([years; years(end:-1:1)],[sfrac(:,na-1); sfrac(end:-1:1,na)], colors(na,:),'EdgeColor','none') 
    end
end
legend(ages, 'location','eastoutside'); set(gca, 'TickLength',[0.003 0.006], 'TickDir', 'out')

clear frac sfrac na ny val colors

%% Swiss - death by age

% load data
opts = delimitedTextImportOptions("NumVariables", 9);
opts.DataLines = [2, Inf]; opts.Delimiter = ";";
opts.VariableNames = ["Year", "Week", "Ending", "Age", "NoDeaths_EP", "Expected", "LowerB", "UpperB", "Diff"];
opts.VariableTypes = ["double", "double", "datetime", "categorical", "double", "double", "double", "double", "categorical"];
opts.ExtraColumnsRule = "ignore"; opts.EmptyLineRule = "read";
opts = setvaropts(opts, ["Age", "Diff"], "EmptyFieldRule", "auto");
opts = setvaropts(opts, "Ending", "InputFormat", "dd.MM.yyyy");
tab = readtable("Swiss weekly deaths 2010-2025 ts-e-14.03.04.03-wr.csv", opts);

tab = tab(~isnan(tab.NoDeaths_EP),:);
tab.Date = tab.Ending - days(3);
tab.Properties.VariableNames{tab.Properties.VariableNames=="NoDeaths_EP"} = 'Count';
tab = tab(:,{'Year','Week','Date','Age','Count'});
clear opts

% preprocessing & normalization
ages = categories(tab.Age); years = unique(tab.Year);
tab.prctDev = nan(size(tab,1),1);
for na = 1:length(ages)
    for nc = 1:length(years)
        idx = tab.Year==years(nc) & tab.Age==ages(na);
        if sum(idx)>=52
            tab.prctDev(idx) = 100*(tab.Count(idx)/sum(tab.Count(idx))*sum(idx) - 1);
        end
    end
end
clear na nc idx

% plot count
colors = parula(6); colors = colors([5; 1],:);
figure; hold on
temp = datetime(years,ones(length(years),1),ones(length(years),1));
plot([temp temp], [0 2000],'color',[1 1 1]*.75, 'HandleVisibility','off');
for na = 1:length(ages)
    idx = tab.Age==ages(na);
    plot(tab.Date(idx), tab.Count(idx),'.', 'Color', colors(na,:));
end
ylabel("Count (deaths)"); xlabel('Year'); legend(ages,'location','eastoutside'); set(gca, 'TickLength',[0.003 0.006], 'TickDir', 'out')
temp = datetime(years,7*ones(length(years),1),ones(length(years),1));
xticks(temp(1:2:end)); xticklabels(years(1:2:end)); ylim([0 2e3]); yticks(0:500:2e3); xlim([datetime(2010,1,1), datetime(max(years),12,30)])

clear idx na temp colors

% plot deviation
figure; hold on
for na = 1:length(ages)
    idx = tab.Age==ages(na) & tab.Year~=2025;
    xx = tab.prctDev(idx); tt = days(tab.Date(idx)-datetime(tab.Year(idx), ones(sum(idx),1), ones(sum(idx),1)));
    subplot(4,6, na); hold on 
    plot([.5 732], [0 0], 'k-'); plot([1 1]*15, [-1 1]*50, 'k-'); plot([1 1]*366+15, [-1 1]*50, 'k-');
    plot(tt, xx, '.', 'MarkerSize',1, 'Color', 'b'); plot(tt+365, xx, '.', 'MarkerSize',3, 'Color', 'b')

    set(gca, 'TickLength',[0.003 0.006], 'TickDir', 'out', 'FontSize',8); xlim([0 732]); xticks(0:365:732); xticklabels(''); ylim([-35 45]); yticks(-40:20:50)

    % peridogram
    tt = days(tab.Date(idx)-datetime(2010,1,1));
    [pxx1, ~] = plomb(xx, tt, [1/365 1/182], 'normalized');
    [pxx, fx] = plomb(xx, tt, 1/7, 'normalized');
    pval = max(fx)*sqrt(4*pi*var(tt))*exp(-pxx1(1)).*sqrt(pxx1(1)); % Baluev method (Eq. 12) in https://doi.org/10.1111/j.1365-2966.2008.12689.x
    pval(pval>1)=1;
    if pval>0.001
        pval = num2str(pval,'%.3f');
    else
        pval = num2str(pval,'%.0e');
    end
    title([ages{na} "p="+pval]);
end
clear tt xx fx pxx pxx1 xx na idx

% plot  fraction of deaths by age
frac = nan(length(years), length(ages));
for ny = 1:length(years)
    val = sum(tab.Count(tab.Year==years(ny)));
    for na = 1:length(ages)
        frac(ny,na) = sum(tab.Count(tab.Year==years(ny) & tab.Age==ages(na)))/val;
    end
end
sfrac = cumsum(frac, 2);
% figure; bar(years, frac, 1, 'stacked', 'EdgeColor','none')
colors = flipud(parula(length(ages)));
figure; hold on
for na = 1:length(ages)
    if na==1
        fill([years; years(end:-1:1)],[zeros(length(years),1); sfrac(end:-1:1,na)], colors(na,:),'EdgeColor','none') %'FaceColor',colors(ny,:),
    else
        fill([years; years(end:-1:1)],[sfrac(:,na-1); sfrac(end:-1:1,na)], colors(na,:),'EdgeColor','none') % 'FaceColor',colors(ny,:)
    end
end
legend(ages, 'location','eastoutside'); set(gca, 'TickLength',[0.003 0.006], 'TickDir', 'out')
clear frac sfrac na ny val colors

%% US - death by underlying cause of death (Chapter level)
% load data
opts = delimitedTextImportOptions("NumVariables", 10);
opts.DataLines = [2, 4536]; opts.Delimiter = ",";
opts.VariableNames = ["Var1", "Year", "Var3", "Var4", "MonthCode", "ICDChapter", "ICDChapterCode", "Deaths", "Var9", "Var10"];
opts.SelectedVariableNames = ["Year", "MonthCode", "ICDChapter", "ICDChapterCode", "Deaths"];
opts.VariableTypes = ["char", "double", "char", "char", "char", "categorical", "categorical", "double", "char", "char"];
opts.ExtraColumnsRule = "ignore"; opts.EmptyLineRule = "read";
opts = setvaropts(opts, ["Var1", "Var3", "Var4", "MonthCode", "Var9", "Var10"], "WhitespaceRule", "preserve");
opts = setvaropts(opts, ["Var1", "Var3", "Var4", "MonthCode", "ICDChapter", "ICDChapterCode", "Var9", "Var10"], "EmptyFieldRule", "auto");

tab = readtable("US Underlying Cause of Death Chapter, 1999-2020.csv", opts);
% format data
temp = cat(1, tab.MonthCode{:}); temp = temp(:,6:7); 
tab.Month = str2num(temp);
tab = removevars(tab, "MonthCode");
tab = tab(tab.Year~=2020,:); % exclude COVID period
clear opts temp

% fraction of deaths by cause
causes = unique(tab(:,{'ICDChapter','ICDChapterCode'}));
causes = sortrows(causes,"ICDChapterCode","ascend");
causes.abbrev = string(causes.ICDChapter);
causes.abbrev = erase(causes.abbrev, "Diseases of the "); 
causes.abbrev = strrep(causes.abbrev, "and", "&"); 

years = unique(tab.Year);
frac = nan(length(years), size(causes,1));
for ny = 1:length(years)
    val = sum(tab.Deaths(tab.Year==years(ny)));
    for na = 1:size(causes,1)
        frac(ny,na) = sum(tab.Deaths(tab.Year==years(ny) & tab.ICDChapterCode==causes.ICDChapterCode(na)))/val;
    end
end
causes.frac_mean = mean(frac)';
causes.fracs = frac';
causes = sortrows(causes,"frac_mean","descend");

sfrac = cumsum(causes.fracs', 2);
colors = rand(size(causes,1), 3); 
figure; hold on
for na = 1:size(causes,1)
    if na==1
        fill([years; years(end:-1:1)],[zeros(length(years),1); sfrac(end:-1:1,na)], colors(na,:),'EdgeColor','none') 
    else
        fill([years; years(end:-1:1)],[sfrac(:,na-1); sfrac(end:-1:1,na)], colors(na,:),'EdgeColor','none')
    end
end
legend(causes.abbrev, 'location','eastoutside'); set(gca, 'TickLength',[0.006 0.012], 'TickDir', 'out'); 
xlim([1999 2019]); xticks(1999:2019); ylim([0 1]); axis ij
clear sfrac frac na ny colors val

% preprocessing & normalization
nDays = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31; ... % n days per month from Jan to Dec (normal year)
    31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]; % n days per month from Jan to Dec (leap year)
tab.prctDev = nan(size(tab,1),1);
for na = 1:size(causes,1)
    for ny = 1:length(years)
        idx = tab.ICDChapterCode==causes.ICDChapterCode(na) & tab.Year==years(ny);
        if sum(idx)==12
            if mod(years(ny), 4)==0 % leap year
                ndays = nDays(2,:)';
            else %normal year
                ndays = nDays(1,:)';
            end
            tab.prctDev(idx) = 100*(tab.Deaths(idx)/sum(tab.Deaths(idx))./ndays*sum(ndays) - 1);
        end
    end
end
clear idx na ny

% plot deviation each chapter - scatter
figure; hold on
count=0;
for na = 1:10%size(causes,1) % top 10
    idx = tab.ICDChapterCode==causes.ICDChapterCode(na) & ~isnan(tab.prctDev);
    if sum(idx)==0; count = count+1; continue; end
    xx = tab.prctDev(idx); tt = tab.Month(idx);
    subplot(4, 6, na); hold on
    plot([.5 24.5], [0 0], 'k-'); plot([1 1]*12.5, [-1 1]*60, 'k-'); 
    plot(tt, xx, '.', 'MarkerSize',1, 'Color', 'b'); plot(tt+12, xx, '.', 'MarkerSize',3, 'Color', 'b')
    set(gca, 'TickLength',[0.006 0.012], 'TickDir', 'out', 'FontSize',8); xlim([0 24.5]); xticks(0:3:24.5); xticklabels(''); ylim([-30 60]); yticks(-40:20:60)

    % peridogram
    xx = xx(:); tt = tab.Year(idx)+(tab.Month(idx)-1)/12;
    [pxx1, ~] = plomb(xx, tt, [1 2], 'normalized');
    [pxx, fx] = plomb(xx, tt, 12, 'normalized');
    pval = max(fx)*sqrt(4*pi*var(tt))*exp(-pxx1(1)).*sqrt(pxx1(1)); % Baluev method (Eq. 12) in https://doi.org/10.1111/j.1365-2966.2008.12689.x
    pval(pval>1)=1;
    if pval>0.001
        pval = num2str(pval,'%.3f');
    else
        pval = num2str(pval,'%.0e');
    end
    title([string(causes.abbrev(na)) "p="+pval]);
end


% plot deviation each chapter - median & ci
figure; hold on
count=0;
for na = 1:10%size(causes,1) % top 10
    idx = tab.ICDChapterCode==causes.ICDChapterCode(na) & ~isnan(tab.prctDev);
    if sum(idx)==0; count = count+1; continue; end
    xx = tab.prctDev(idx); tt = tab.Month(idx);
    subplot(4, 6, na); hold on
    plot([.5 24.5], [0 0], 'k-'); plot([1 1]*12.5, [-1 1]*60, 'k-'); 
    xx = reshape(xx, 12, length(years));
    avg = median(xx, 2)'; 
    ci = mad(xx,1,2)'*1.4826/sqrt(size(xx,2))*tinv(0.975, size(xx,2)-1); % std(xx, 2)
    fill([1:24 24:-1:1], [avg-ci avg-ci avg(end:-1:1)+ci(end:-1:1) avg(end:-1:1)+ci(end:-1:1)], 'b', 'facealpha', .25, 'edgecolor','none' )
    plot(1:24, [avg avg], 'b', 'LineWidth',1)
    set(gca, 'TickLength',[0.006 0.012], 'TickDir', 'out', 'FontSize',8); xlim([0 24.5]); xticks(0:3:24.5); xticklabels(''); ylim([-25 30]); yticks(-40:20:60)

    % peridogram
    xx = xx(:); tt = tab.Year(idx)+(tab.Month(idx)-1)/12;
    [pxx1, ~] = plomb(xx, tt, [1 2], 'normalized');
    [pxx, fx] = plomb(xx, tt, 12, 'normalized');
    pval = max(fx)*sqrt(4*pi*var(tt))*exp(-pxx1(1)).*sqrt(pxx1(1)); % Baluev method (Eq. 12) in https://doi.org/10.1111/j.1365-2966.2008.12689.x
    pval(pval>1)=1;
    if pval>0.001
        pval = num2str(pval,'%.3f');
    else
        pval = num2str(pval,'%.0e');
    end
    title([string(causes.abbrev(na)) "p="+pval]);
end