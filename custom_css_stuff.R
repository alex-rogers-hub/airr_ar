# Custom CSS
custom_css <- "
@import url('https://fonts.googleapis.com/css2?family=Poppins:wght@300;400;600;700&display=swap');

* {
  font-family: 'Poppins', 'Segoe UI', Arial, sans-serif;
}

body {
  background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
  min-height: 100vh;
}

.login-container {
  max-width: 450px;
  margin: 80px auto;
  background: white;
  border-radius: 20px;
  box-shadow: 0 20px 60px rgba(0,0,0,0.3);
  overflow: hidden;
  animation: slideDown 0.5s ease-out;
}

@keyframes slideDown {
  from {
    opacity: 0;
    transform: translateY(-50px);
  }
  to {
    opacity: 1;
    transform: translateY(0);
  }
}

.login-header {
  background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
  color: white;
  padding: 40px 30px;
  text-align: center;
}

.login-header h2 {
  margin: 0;
  font-weight: 600;
  font-size: 28px;
}

.login-header p {
  margin: 10px 0 0 0;
  opacity: 0.9;
  font-size: 14px;
}

.login-body {
  padding: 40px 30px;
}

.form-group {
  margin-bottom: 25px;
}

.form-group label {
  color: #2C3E50;
  font-weight: 500;
  margin-bottom: 8px;
  display: block;
}

.form-control {
  width: 100%;
  padding: 12px 15px;
  border: 2px solid #E8EDF2;
  border-radius: 10px;
  font-size: 14px;
  transition: all 0.3s;
  box-sizing: border-box;
}

.form-control:focus {
  border-color: #667eea;
  outline: none;
  box-shadow: 0 0 0 3px rgba(102, 126, 234, 0.1);
}

.btn-login, .btn-register {
  width: 100%;
  padding: 14px;
  border: none;
  border-radius: 10px;
  font-size: 16px;
  font-weight: 600;
  cursor: pointer;
  transition: all 0.3s;
  margin-bottom: 10px;
}

.btn-login {
  background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
  color: white;
}

.btn-login:hover {
  transform: translateY(-2px);
  box-shadow: 0 10px 20px rgba(102, 126, 234, 0.3);
}

.btn-register {
  background: white;
  color: #667eea;
  border: 2px solid #667eea;
}

.btn-register:hover {
  background: #f8f9ff;
}

.toggle-link {
  text-align: center;
  margin-top: 20px;
  color: #7f8c8d;
  font-size: 14px;
}

.toggle-link a {
  color: #667eea;
  cursor: pointer;
  font-weight: 600;
  text-decoration: none;
}

.toggle-link a:hover {
  text-decoration: underline;
}

.alert {
  padding: 12px 15px;
  border-radius: 10px;
  margin-bottom: 20px;
  font-size: 14px;
}

.alert-danger {
  background: #fee;
  color: #c33;
  border: 1px solid #fcc;
}

.alert-success {
  background: #efe;
  color: #3c3;
  border: 1px solid #cfc;
}

/* Dashboard Styles */
.skin-blue .main-header .navbar {
  background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
}

.skin-blue .main-header .logo {
  background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
  font-weight: 600;
  font-size: 20px;
}

.skin-blue .main-header .logo:hover {
  background: linear-gradient(135deg, #5568d3 0%, #653a8b 100%);
}

.content-wrapper {
  background: #f4f6f9;
}

.box {
  border-radius: 10px;
  box-shadow: 0 2px 10px rgba(0,0,0,0.08);
  border-top: none;
  margin-bottom: 25px;
}

.box-header {
  border-radius: 10px 10px 0 0;
  padding: 20px;
}

.box-header.with-border {
  border-bottom: 2px solid #f4f6f9;
}

.small-box {
  border-radius: 10px;
  box-shadow: 0 2px 10px rgba(0,0,0,0.08);
  transition: all 0.3s;
}

.small-box:hover {
  transform: translateY(-5px);
  box-shadow: 0 10px 25px rgba(0,0,0,0.15);
}

.small-box > .inner {
  padding: 15px;
}

.small-box h3 {
  font-size: 38px;
  font-weight: 700;
  margin: 0 0 10px 0;
}

.small-box p {
  font-size: 14px;
  font-weight: 500;
}

/* Loading spinner */
.loading-spinner {
  position: fixed;
  top: 50%;
  left: 50%;
  transform: translate(-50%, -50%);
  z-index: 9999;
}

.spinner {
  border: 4px solid #f3f3f3;
  border-top: 4px solid #667eea;
  border-radius: 50%;
  width: 50px;
  height: 50px;
  animation: spin 1s linear infinite;
}

@keyframes spin {
  0% { transform: rotate(0deg); }
  100% { transform: rotate(360deg); }
}

/* Plotly container */
.plotly-container {
  background: white;
  border-radius: 10px;
  padding: 15px;
  box-shadow: 0 2px 10px rgba(0,0,0,0.08);
}

/* Value boxes gradient */
.bg-aqua-gradient {
  background: linear-gradient(135deg, #00c6ff 0%, #0072ff 100%) !important;
}

.bg-green-gradient {
  background: linear-gradient(135deg, #11998e 0%, #38ef7d 100%) !important;
}

.bg-yellow-gradient {
  background: linear-gradient(135deg, #f093fb 0%, #f5576c 100%) !important;
}

.bg-red-gradient {
  background: linear-gradient(135deg, #fa709a 0%, #fee140 100%) !important;
}
"
