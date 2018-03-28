#!/usr/bin/Rscript

initial_dir = getwd()
setwd("C:/Users/klosa/Desktop/Seattle_Documentation/5.2-Modified sparse-group lasso")

#******************************************************************************
#*  Read data and determine some main parameters.                             *
#******************************************************************************
X        = as.matrix(read.table("X_sim.txt", header = FALSE, sep = " ", dec = "."))
beta_sim = as.matrix(read.table("beta_sim.txt", header = FALSE, sep = " ", dec = "."))
y        = as.matrix(read.table("y_sim.txt", header = FALSE, sep = " ", dec = "."))

n              = dim(X)[1]
p              = dim(X)[2]
n_groups       = 3
lambda         = 0
lambda_max     = 0
omega_group    = rep(1,n_groups)
alpha          = 0.95
gamma          = 0.6
eps_rel        = 0.0001
max_iter       = 1000
proportion_xi  = 0.1
num_intervals  = 20
epsilon_max    = 0.01
epsilon_min    = 0.000001
num_interv_eps = 5

#******************************************************************************
#*  Create groups.                                                            *
#******************************************************************************
group_sizes = as.integer(rep(0,n_groups))
index_start = as.integer(rep(1,n_groups))
index_end   = as.integer(rep(p,n_groups))

group_sizes[1] = 3
group_sizes[2] = 4
group_sizes[3] = 3

index_start[1] = 1
index_start[2] = 4
index_start[3] = 8

index_end[1] = 3
index_end[2] = 7
index_end[3] = 10

#members     = as.integer(round(p/n_groups))
#left_over   = p
#for (i in 1:(n_groups-1)) {
#  group_sizes[i] = as.integer(members)
#  left_over      = left_over-members
#}
#group_sizes[n_groups] = as.integer(left_over)
#group_sizes = sample(group_sizes, n_groups, replace=FALSE)
#for (i in 1:(n_groups-1)) {
#  index_start[i+1] = as.integer(index_start[i] + group_sizes[i])
#  index_end[i]     = as.integer(index_start[i+1] - 1)
#}

#******************************************************************************
#*  Soft-thresholding operator, arguments: vector, scalar.                    *
#******************************************************************************
soft_thresh_op = function(vector, scalar) {
  p_sto      = length(vector)
  result_sto = rep(0,p_sto)
  for (i in 1:p_sto) {
    if (vector[i] > scalar[i]) {
      result_sto[i] = vector[i] - scalar[i]
    } else if (vector[i] < -scalar[i]) {
      result_sto[i] = vector[i] + scalar[i]
    } else {
      result_sto[i] = 0
    }
  }
  return(result_sto)
}

#******************************************************************************
#*  l2-norm, argument: vector.                                                *
#******************************************************************************
l2_norm = function(vector) {
  return(sqrt(t(vector)%*%vector))
}

#******************************************************************************
#*  Squared l2-norm, argument: vector.                                        *
#******************************************************************************
l2_norm_squared = function(vector) {
  return(t(vector)%*%vector)
}

#******************************************************************************
#*  l infinty-norm, argument: vector.                                         *
#******************************************************************************
l_inf_norm = function(vector) {
  return(max(abs(vector)))
}

#******************************************************************************
#*  Function g_epsilon, argument: vector (X^k * beta^k), scalar (epsilon).    *
#******************************************************************************
function_g_eps = function(vector, scalar) {
  temp = l2_norm_squared(vector)
  if (temp > scalar) {
    return(sqrt(temp))
  } else {
    return(0.5*(temp/scalar + scalar))
  }
}

#******************************************************************************
#*  Sum over groups of new function g_epsilon, argument: vector (beta),       *
#*  scalar (epsilon).                                                         *
#******************************************************************************
sum_g_eps = function(vector, scalar) {
  temp     = 0
  temp_vec = rep(0,n)
  for (k in 1:n_groups) {
    temp_vec = X[,index_start[k]:index_end[k]]%*%vector[index_start[k]:index_end[k]]
    temp = temp + sqrt(group_sizes[k])*omega_group[k]*function_g_eps(temp_vec, scalar)
  }
  rm(temp_vec)
  return(temp*n*(1-alpha)*lambda)
}

#******************************************************************************
#*  Scaled gradient of g_epsilon, argument: vector (beta), scalar (epsilon).  *
#******************************************************************************
gradient_g_eps = function(vector, scalar) {
  temp1  = 0
  temp2  = 0
  result = rep(0,length(vector))
  for (k in 1:n_groups) {
    temp_vec1 = rep(0,n)
    temp_vec2 = rep(0,group_sizes[k])
    temp_vec1 = X[,index_start[k]:index_end[k]]%*%vector[index_start[k]:index_end[k]]
    temp1     = l2_norm(temp_vec1)
    temp2     = n*(1-alpha)*lambda*sqrt(group_sizes[k])*omega_group[k]
    temp_vec2 = t(X[,index_start[k]:index_end[k]])%*%temp_vec1
    if (temp1 > scalar) {
      for (j in 1:group_sizes[k]) {
        result[index_start[k]+j-1] = (temp2/temp1)*temp_vec2[j]
      }
    } else {
      for (j in 1:group_sizes[k]) {
        result[index_start[k]+j-1] = (temp2/scalar)*temp_vec2[j]
      }
    }
    rm(temp_vec1)
    rm(temp_vec2)
  }
  return(result)
}

#******************************************************************************
#*  Function g (f = g + h), argument: vector, scalar.                         *
#******************************************************************************
sys_function_g = function(vector, scalar) {
  return(0.5*l2_norm_squared(y - X%*%vector) + sum_g_eps(vector, scalar))
}

#******************************************************************************
#*  Non-repeatable calculations, and further initializations.                 *
#******************************************************************************
Xty        = t(X)%*%y
XtX        = t(X)%*%X
beta       = rep(0, p)
beta_new   = rep(0, p)

#******************************************************************************
#*  Calculations concerning the weights omega.                                *
#******************************************************************************
omega_group[1]  = 0
omega_group[2]  = 1
omega_group[3]  = 1
omega_feature   = rep(2,p)
counter_feature = as.integer(1)
num_zeros_feat  = 0

# 'Translate' omega_group to omega_feature:
for (i in 1:n_groups) {
  if (omega_group[i] == 0) {
    for (j in 1:group_sizes[i]) {
      num_zeros_feat = num_zeros_feat + 1
      omega_feature[counter_feature] = omega_group[i]
      counter_feature = as.integer(counter_feature + 1)
    }
  } else {
    for (j in 1:group_sizes[i]) {
      omega_feature[counter_feature] = omega_group[i]
      counter_feature = as.integer(counter_feature + 1)
    }
  }
}

X_A = matrix(1, nrow=n, ncol=num_zeros_feat)
if (num_zeros_feat > 0) {
  counter = as.integer(1)
  for (i in 1:p) {
    if (omega_feature[i] == 0) {
      X_A[, counter] = X[, i]
      counter = as.integer(counter + 1)
    }
  }
  
  beta_A  = solve(t(X_A)%*%X_A)%*%t(X_A)%*%y
  counter = as.integer(1)
  for (i in 1:p) {
    if (omega_feature[i] == 0) {
      beta[i] = beta_A[counter]
      counter = as.integer(counter + 1)
    }
  }
  res_A = y - X_A%*%beta_A
} else {
  res_A = y
}

#******************************************************************************
#*  Calculation of lambda_max.                                                *
#******************************************************************************
Xtres_A = t(X)%*%res_A
max_groups = rep(0, n_groups)
for (k in 1:n_groups) {
  if (omega_group[k] != 0) {
    max_groups[k] = max(abs(Xtres_A[index_start[k]:index_end[k]]))/(alpha*n*omega_group[k])
  }
}
lambda_max = max(max_groups)
lambda_max = lambda_max*1.00001

#******************************************************************************
#*  Proximal gradient descent with backtracking line search.                  *
#******************************************************************************
for (i in 1:num_intervals) {
  lambda = lambda_max*proportion_xi^((i-1)/(num_intervals-1))
  
  for (j in 1:num_interv_eps) {
    accuracy  = FALSE
    iteration = 1
    epsilon   = epsilon_max*(epsilon_min/epsilon_max)^((j-1)/(num_interv_eps-1))
    
    while (!accuracy && (iteration<=max_iter)) {
      grad      = XtX%*%beta - Xty + gradient_g_eps(beta,epsilon)
      criterion = FALSE
      t         = 1
      
      while (!criterion) {
        beta_new = soft_thresh_op(beta-t*grad, alpha*lambda*n*t*omega_feature)
        temp = sys_function_g(beta,epsilon) - t(grad)%*%(beta-beta_new) + (1/(2*t))*l2_norm_squared(beta-beta_new)
        if (sys_function_g(beta_new,epsilon) > temp) {
          t = gamma*t
        } else {
          if (l_inf_norm(beta_new - beta) <= eps_rel*l2_norm(beta)) {
            accuracy = TRUE
          }
          beta = beta_new
          criterion = TRUE
        }
      }
      iteration = iteration + 1
    }
    
#******************************************************************************
#*  Write output.                                                             *
#******************************************************************************
    out_file_beta = "modified_sg_lasso_output_R_beta.txt"
    out_file_iter = "modified_sg_lasso_output_R_iter.txt"
    if ((i == 1) && (j == 1)) {
      write.table(t(beta), file = out_file_beta, append = FALSE, quote = FALSE, sep = " ",
                  eol = "\n", na = "NA", dec = ".", row.names = FALSE,
                  col.names = FALSE)
      write.table(as.integer(iteration-1), file = out_file_iter, append = FALSE, quote = FALSE, sep = " ",
                  eol = "\n", na = "NA", dec = ".", row.names = FALSE,
                  col.names = FALSE)
    } else {
      write.table(t(beta), file = out_file_beta, append = TRUE, quote = FALSE, sep = " ",
                  eol = "\n", na = "NA", dec = ".", row.names = FALSE,
                  col.names = FALSE)
      write.table(as.integer(iteration-1), file = out_file_iter, append = TRUE, quote = FALSE, sep = " ",
                  eol = "\n", na = "NA", dec = ".", row.names = FALSE,
                  col.names = FALSE)
    }
  }
}

out_file_lambda = "modified_sg_lasso_output_R_lambda.txt"
write.table(lambda_max, file = out_file_lambda, append = FALSE, quote = FALSE, sep = " ",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE,
            col.names = FALSE)

setwd(initial_dir)
