% izberemo si nekaj različnih vrednosti za alfo

alphe = [0.3; 0.5; 1; 3; 7; 10; 15; 20];

% na isto sliko narišemo graf za vse izbrane alfe, da jih lahko primerjamo
for i = 1:(length(alphe))
    alpha = alphe(i);
    f = @(x)   (gamma(2 .* alpha) / gamma(alpha)^2 ) .* (x .* (1 - x)).^(alpha - 1);

    x = linspace(0, 1, 200);
    plot(x, f(x), 'LineWidth', 1.5);
    title("Grafi v odvisnosti od alfe");
    legend("0.3", "0.5", "1", "3", "7", "10", "15", "20");
    hold on
end
