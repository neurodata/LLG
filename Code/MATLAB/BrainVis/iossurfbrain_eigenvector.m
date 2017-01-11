clear all
% addpath('E:/GitHub/LLG/Code/MATLAB/BrainVis/NII')
addpath('./NII')

fileName = '../../../Result/eigenvector.csv';
xHat = reshape(csvread(fileName), 70, 2);
xHat = xHat(:, 2);
max(xHat)
min(xHat)

% Load atlas
mask = load_nii('desikan.nii');
mask.img = double(mask.img);
maskCopy = mask.img;

% Get brain background
brainBackground = 1*((mask.img~=0));
brainBackground(brainBackground==0)=NaN;

mask.img((mask.img~=0))=0;

%%
n = length(unique(maskCopy))-1;
centroids = zeros(n,3);
for x = 1:size(maskCopy,1)
    for y = 1:size(maskCopy,2)
        for z = 1:size(maskCopy,3)
            if (maskCopy(x,y,z)~=0)
                centroids(maskCopy(x,y,z),:) = ...
                    centroids(maskCopy(x,y,z),:)+[x,y,z];
            end
        end
    end
end
for i = 1:n
    centroids(i,:) = centroids(i,:)/sum(sum(sum(maskCopy==i)));
end


%%

figure(1);

p1 = subplot(1,2,1);

FV = {};
FV{1} = isosurface(double(maskCopy~=0), .5);
% colors = distinguishable_colors(71,'w');
p = patch(FV{1});
isonormals(double(maskCopy~=0),p)
p.FaceColor = [0, 0, 0];
p.EdgeColor = 'none';
p.FaceAlpha = .15;
daspect([1,1,1])
view([1,0,0]); axis tight
lighting gouraud
material dull
light('Position', [100 100 100], 'Style', 'local')
set(gca,'Visible','off')
hold on

for i = 1:70
    V = double(maskCopy == i);
    FV{i+1} = isosurface(V, .5);
    p = patch(FV{i+1});
    isonormals(double(maskCopy~=0),p)
    p.FaceColor = [1, 1, (xHat(i)+1)/2];
    p.EdgeColor = 'none';
    p.FaceAlpha = 0.2;
    lighting gouraud
    material dull
end
colorbar

hold off




p2 = subplot(1,2,2);

FV = {};
FV{1} = isosurface(double(maskCopy~=0), .5);
colors = distinguishable_colors(21,'w');
p = patch(FV{1});
isonormals(double(maskCopy~=0),p)
p.FaceColor = colors(1,:);
p.EdgeColor = 'none';
p.FaceAlpha = .15;
daspect([1,1,1])
view([0,0,1]); axis tight
lighting gouraud
material dull
light('Position', [100 100 100], 'Style', 'local')
set(gca,'Visible','off')
hold on

for i = 1:length(vertexVec)
    V = double(mask.img == vertexVec(i));
    FV{i+1} = isosurface(V, .5);
    p = patch(FV{i+1});
    isonormals(double(maskCopy~=0),p)
    p.FaceColor = colors(i+1,:);
    p.EdgeColor = 'none';
    p.FaceAlpha = 1;
    lighting gouraud
    material dull
end

hold on;

for i = 1:size(edgeVec,1)
    cen = [centroids((edgeVec(i,1)),:);
        centroids((edgeVec(i,2)),:)];
    if (edgeMagnitude(i) > 0)
        pp3 = plot3(cen(:,2), cen(:,1), cen(:,3), '-r', ...
            'LineWidth', edgeMagnitude(i));
    else
        pp4 = plot3(cen(:,2), cen(:,1), cen(:,3), '-b', ...
            'LineWidth', -edgeMagnitude(i));
    end
end
hold off

hL = legend([pp1,pp2], {'Overestimate','Underestimate'}, ...
    'Location','South');
set(hL,'FontSize',20)

p1Pos = get(p1,'position');
p2Pos = get(p2,'position');
p2Pos(2:4) = p1Pos(2:4);
set(p2, 'position', p2Pos);

% if (model == 1)
%     figureName = '../../../Result/Diff_Between_desikan.png';
% elseif (model == 2)
%     figureName = '../../../Result/Diff_Abar_desikan.png';
% else
%     figureName = '../../../Result/Diff_Phat_desikan.png';
% end
% 
% r = 150; % pixels per inch
% set(gcf, 'PaperUnits', 'inches', 'PaperPosition', [0 0 1080 480]/r);
% print(gcf,'-dpng',sprintf('-r%d',r), figureName);

