function [theta, J_history] = gradientDescent(X, y, theta, alpha, num_iters)
%GRADIENTDESCENT Performs gradient descent to learn theta
%   theta = GRADIENTDESCENT(X, y, theta, alpha, num_iters) updates theta by 
%   taking num_iters gradient steps with learning rate alpha

% Initialize some useful values
m = length(y); % number of training examples
J_history = zeros(num_iters, 1);

for iter = 1:num_iters

    % ====================== YOUR CODE HERE ======================
    % Instructions: Perform a single gradient step on the parameter vector
    %               theta. 
    %
    % Hint: While debugging, it can be useful to print out the values
    %       of the cost function (computeCost) and gradient here.
    
    % theta_j := theta_j - sum((theta_0 + theta_1 * x_i - y_i)^2 * x_j) / m
    
    theta = theta - alpha * (X' * (X * theta - y)) / m;
    %theta(1) = theta(1) - alpha * sum((X * theta - y) .* X(:, 1)) / m;
    %theta(2) = theta(2) - alpha * sum((X * theta - y) .* X(:, 2)) / m;
    % ============================================================

    % Save the cost J in every iteration    
    J_history(iter) = computeCost(X, y, theta);

end

end
