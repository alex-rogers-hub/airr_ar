custom_css <- paste0("
/* ============================================
   FONT
   ============================================ */
@import url('https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&display=swap');

* {
  font-family: 'Inter', -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
}

/* ============================================
   GLOBAL
   ============================================ */
body, .content-wrapper, .right-side {
  background-color: #F5F5F0 !important;
}

/* ============================================
   HEADER
   ============================================ */
.main-header .navbar {
  background: #1A1A1A !important;
  border-bottom: 2px solid #D4A843;
}

.main-header .logo {
  background: #1A1A1A !important;
  color: #F5F5F0 !important;
  font-weight: 700;
  font-size: 20px;
  letter-spacing: 2px;
  border-bottom: none !important;
}

.main-header .logo:hover {
  background: #2D2D2D !important;
}

.main-header .sidebar-toggle {
  color: #F5F5F0 !important;
}

.main-header .sidebar-toggle:hover {
  background: #2D2D2D !important;
}

.main-header .navbar .nav > li > a {
  color: #F5F5F0 !important;
}

/* ============================================
   SIDEBAR
   ============================================ */
.main-sidebar, .left-side {
  background-color: #1A1A1A !important;
}

.sidebar-menu > li > a {
  color: #9E9E9E !important;
  font-weight: 500;
  font-size: 14px;
  padding: 14px 20px;
  border-left: 3px solid transparent;
  transition: all 0.2s ease;
}

.sidebar-menu > li.active > a {
  border-left: 3px solid #D4A843 !important;
  background-color: rgba(212, 168, 67, 0.1) !important;
  color: #D4A843 !important;
}

.sidebar-menu > li > a:hover {
  background-color: rgba(255, 255, 255, 0.05) !important;
  color: #F5F5F0 !important;
}

.sidebar-menu > li > a > .fa,
.sidebar-menu > li > a > .glyphicon {
  color: inherit;
}

/* Sidebar user panel */
.sidebar .user-panel {
  border-bottom: 1px solid #2D2D2D;
}

/* ============================================
   BOXES
   ============================================ */
.box {
  border-radius: 12px;
  border: none;
  box-shadow: 0 1px 8px rgba(0, 0, 0, 0.06);
  background: white;
  transition: box-shadow 0.2s ease;
}

.box:hover {
  box-shadow: 0 2px 16px rgba(0, 0, 0, 0.1);
}

.box-header {
  border-bottom: 1px solid rgba(0, 0, 0, 0.06);
  padding: 16px 20px;
}

.box-header .box-title {
  font-size: 13px;
  font-weight: 600;
  text-transform: uppercase;
  letter-spacing: 0.5px;
  color: #4a5568;
}

.box.box-primary > .box-header {
  background: #1A1A1A;
  border-radius: 12px 12px 0 0;
  border-bottom: 2px solid #D4A843;
}

.box.box-primary > .box-header .box-title {
  color: #F5F5F0;
}

.box.box-primary {
  border-top: none;
}

.box.box-info > .box-header {
  background: #1A1A1A;
  border-radius: 12px 12px 0 0;
  border-bottom: 2px solid #3498DB;
}

.box.box-info > .box-header .box-title {
  color: #F5F5F0;
}

.box.box-info {
  border-top: none;
}

.box.box-success > .box-header {
  background: #1A1A1A;
  border-radius: 12px 12px 0 0;
  border-bottom: 2px solid #27AE60;
}

.box.box-success > .box-header .box-title {
  color: #F5F5F0;
}

.box.box-success {
  border-top: none;
}

.box.box-warning > .box-header {
  background: #1A1A1A;
  border-radius: 12px 12px 0 0;
  border-bottom: 2px solid #D4A843;
}

.box.box-warning > .box-header .box-title {
  color: #F5F5F0;
}

.box.box-warning {
  border-top: none;
}

.box-body {
  padding: 20px;
}

/* ============================================
   SCORE CARDS
   ============================================ */
.score-card-main {
  background: #1A1A1A;
  border-radius: 16px;
  padding: 30px;
  color: #F5F5F0;
  text-align: center;
  height: 100%;
  min-height: 200px;
  display: flex;
  flex-direction: column;
  justify-content: center;
  box-shadow: 0 4px 20px rgba(0, 0, 0, 0.15);
  border-bottom: 3px solid #D4A843;
}

.score-card-main .score-value {
  font-size: 52px;
  font-weight: 700;
  line-height: 1;
  margin: 10px 0;
  color: #D4A843;
}

.score-card-main .score-label {
  font-size: 13px;
  font-weight: 600;
  text-transform: uppercase;
  letter-spacing: 1.5px;
  opacity: 0.8;
}

.score-card-grid {
  display: grid;
  grid-template-columns: 1fr 1fr;
  grid-template-rows: 1fr 1fr;
  gap: 10px;
  height: 100%;
  min-height: 200px;
}

.score-card-mini {
  border-radius: 12px;
  padding: 18px;
  text-align: center;
  display: flex;
  flex-direction: column;
  justify-content: center;
  box-shadow: 0 2px 8px rgba(0, 0, 0, 0.08);
  transition: transform 0.2s ease, box-shadow 0.2s ease;
}

.score-card-mini:hover {
  transform: translateY(-2px);
  box-shadow: 0 4px 16px rgba(0, 0, 0, 0.15);
}

.score-card-mini .score-value {
  font-size: 26px;
  font-weight: 700;
  line-height: 1;
  margin: 5px 0;
}

.score-card-mini .score-label {
  font-size: 10px;
  font-weight: 600;
  text-transform: uppercase;
  letter-spacing: 0.5px;
  opacity: 0.9;
}

.score-card-mini.presence {
  background: #1A1A1A;
  color: #27AE60;
  border-bottom: 2px solid #27AE60;
}

.score-card-mini.perception {
  background: #1A1A1A;
  color: #D4A843;
  border-bottom: 2px solid #D4A843;
}

.score-card-mini.prestige {
  background: #1A1A1A;
  color: #8E44AD;
  border-bottom: 2px solid #8E44AD;
}

.score-card-mini.persistence {
  background: #1A1A1A;
  color: #2980B9;
  border-bottom: 2px solid #2980B9;
}

/* ============================================
   VALUE BOXES (used in comparisons tab etc)
   ============================================ */
.small-box {
  border-radius: 12px;
  overflow: hidden;
  box-shadow: 0 2px 12px rgba(0, 0, 0, 0.08);
  min-height: 100px;
}

.small-box h3 {
  font-size: 28px;
  font-weight: 700;
}

.small-box p {
  font-size: 12px;
  font-weight: 500;
  text-transform: uppercase;
  letter-spacing: 0.5px;
}

/* ============================================
   DATATABLES
   ============================================ */
.dataTables_wrapper {
  padding: 0;
}

table.dataTable {
  border-collapse: collapse !important;
  font-size: 13px;
  width: 100% !important;
}

table.dataTable thead th {
  background: #1A1A1A !important;
  color: #F5F5F0 !important;
  font-weight: 600;
  font-size: 11px;
  text-transform: uppercase;
  letter-spacing: 0.5px;
  padding: 12px 15px !important;
  border-bottom: 2px solid #D4A843 !important;
}

table.dataTable thead th:first-child {
  border-radius: 8px 0 0 0;
}

table.dataTable thead th:last-child {
  border-radius: 0 8px 0 0;
}

table.dataTable tbody td {
  padding: 10px 15px !important;
  border-bottom: 1px solid #f0f0f0 !important;
  vertical-align: middle;
  color: #2D2D2D;
}

table.dataTable tbody tr:hover {
  background-color: rgba(212, 168, 67, 0.04) !important;
}

table.dataTable tbody tr:nth-child(even) {
  background-color: #fafaf7;
}

table.dataTable tbody tr:last-child td:first-child {
  border-radius: 0 0 0 8px;
}

table.dataTable tbody tr:last-child td:last-child {
  border-radius: 0 0 8px 0;
}

.dataTables_length select,
.dataTables_filter input {
  border: 1px solid #e2e8f0;
  border-radius: 6px;
  padding: 6px 10px;
  font-size: 13px;
}

.dataTables_info {
  font-size: 12px;
  color: #9E9E9E;
  padding-top: 12px !important;
}

.dataTables_paginate .paginate_button {
  border-radius: 6px !important;
  font-size: 12px !important;
}

.dataTables_paginate .paginate_button.current {
  background: #D4A843 !important;
  color: #1A1A1A !important;
  border-color: #D4A843 !important;
  font-weight: 600;
}

.dataTables_paginate .paginate_button:hover {
  background: #B8922E !important;
  color: #1A1A1A !important;
  border-color: #B8922E !important;
}

/* ============================================
   LOGIN / REGISTER
   ============================================ */
.login-container {
  max-width: 420px;
  margin: 60px auto;
  border-radius: 16px;
  overflow: hidden;
  box-shadow: 0 10px 40px rgba(0, 0, 0, 0.2);
  background: #ffffff;
}

.login-header {
  background: #1A1A1A;
  padding: 35px 30px 25px;
  text-align: center;
  color: #F5F5F0;
  border-bottom: 3px solid #D4A843;
}

.login-header h2 {
  margin: 0 0 8px 0;
  font-weight: 700;
  font-size: 24px;
  color: #F5F5F0;
}

.login-header p {
  margin: 0;
  opacity: 0.7;
  font-size: 14px;
  color: #9E9E9E;
}

.login-body {
  padding: 30px;
}

.login-body .form-group {
  margin-bottom: 16px;
}

.login-body .form-control {
  height: 44px;
  border: 1.5px solid #e2e8f0;
  border-radius: 8px;
  padding: 10px 14px;
  font-size: 14px;
  transition: border-color 0.2s ease, box-shadow 0.2s ease;
}

.login-body .form-control:focus {
  border-color: #D4A843;
  box-shadow: 0 0 0 3px rgba(212, 168, 67, 0.15);
  outline: none;
}

.btn-login, .btn-register {
  width: 100%;
  height: 44px;
  border: none;
  border-radius: 8px;
  font-weight: 600;
  font-size: 15px;
  cursor: pointer;
  transition: all 0.2s ease;
  background: #1A1A1A;
  color: #D4A843;
  margin-top: 8px;
  border: 2px solid #D4A843;
}

.btn-login:hover, .btn-register:hover {
  background: #D4A843;
  color: #1A1A1A;
  transform: translateY(-1px);
  box-shadow: 0 4px 15px rgba(212, 168, 67, 0.3);
}

.toggle-link {
  text-align: center;
  margin-top: 20px;
  font-size: 13px;
  color: #9E9E9E;
}

.toggle-link a {
  color: #D4A843;
  cursor: pointer;
  font-weight: 600;
  text-decoration: none;
}

.toggle-link a:hover {
  text-decoration: underline;
  color: #B8922E;
}

.alert {
  border-radius: 8px;
  font-size: 13px;
  padding: 10px 14px;
  margin-bottom: 16px;
  border: none;
}

/* ============================================
   BUTTONS
   ============================================ */
.btn-primary {
  background: #D4A843;
  border: none;
  border-radius: 8px;
  font-weight: 600;
  color: #1A1A1A;
  transition: all 0.2s ease;
}

.btn-primary:hover, .btn-primary:focus, .btn-primary:active {
  background: #B8922E !important;
  color: #1A1A1A !important;
  transform: translateY(-1px);
  box-shadow: 0 4px 15px rgba(212, 168, 67, 0.3);
}

.btn-success {
  border-radius: 8px;
  font-weight: 600;
}

.btn-danger {
  border-radius: 8px;
  font-weight: 500;
  background: #C0392B;
  border-color: #C0392B;
}

.btn-sm {
  border-radius: 6px;
  font-size: 12px;
  padding: 4px 12px;
}

.btn-download {
  background: #1A1A1A;
  color: #D4A843;
  border: 1px solid #D4A843;
  border-radius: 8px;
  font-weight: 500;
  font-size: 13px;
  padding: 8px 16px;
  transition: all 0.2s ease;
}

.btn-download:hover {
  background: #D4A843;
  color: #1A1A1A;
  transform: translateY(-1px);
}

/* ============================================
   TAB PANELS
   ============================================ */
.nav-tabs {
  border-bottom: 2px solid #e2e8f0;
}

.nav-tabs > li > a {
  font-size: 13px;
  font-weight: 500;
  color: #9E9E9E;
  border: none;
  padding: 10px 18px;
  transition: color 0.2s ease;
}

.nav-tabs > li > a:hover {
  border: none;
  background: transparent;
  color: #D4A843;
}

.nav-tabs > li.active > a,
.nav-tabs > li.active > a:hover,
.nav-tabs > li.active > a:focus {
  border: none;
  border-bottom: 2px solid #D4A843;
  color: #D4A843;
  font-weight: 600;
  background: transparent;
}

/* ============================================
   FORM INPUTS
   ============================================ */
.form-control {
  border: 1.5px solid #e2e8f0;
  border-radius: 8px;
  padding: 8px 12px;
  font-size: 14px;
  transition: border-color 0.2s ease;
}

.form-control:focus {
  border-color: #D4A843;
  box-shadow: 0 0 0 3px rgba(212, 168, 67, 0.1);
}

.selectize-input {
  border: 1.5px solid #e2e8f0 !important;
  border-radius: 8px !important;
  box-shadow: none !important;
}

.selectize-input.focus {
  border-color: #D4A843 !important;
  box-shadow: 0 0 0 3px rgba(212, 168, 67, 0.1) !important;
}

/* ============================================
   CHART LAYOUT (flex row)
   ============================================ */
.chart-row-flex {
  display: flex;
  gap: 20px;
  flex-wrap: wrap;
}

.chart-row-flex > .chart-col-trend {
  flex: 1 1 60%;
  min-width: 300px;
}

.chart-row-flex > .chart-col-spider {
  flex: 1 1 35%;
  min-width: 280px;
}

@media (max-width: 768px) {
  .chart-row-flex > .chart-col-trend,
  .chart-row-flex > .chart-col-spider {
    flex: 1 1 100%;
  }
  
  .score-card-main .score-value {
    font-size: 40px;
  }
  
  .score-card-mini .score-value {
    font-size: 20px;
  }
}

/* ============================================
   SPINNERS
   ============================================ */
.shiny-spinner-output-container {
  min-height: 100px;
}

/* ============================================
   NOTIFICATIONS
   ============================================ */
.shiny-notification {
  border-radius: 10px;
  box-shadow: 0 4px 20px rgba(0, 0, 0, 0.15);
  border: none;
  font-size: 13px;
  border-left: 4px solid #D4A843;
}

/* ============================================
   COLLAPSIBLE BOX TOGGLE
   ============================================ */
.box-header > .box-tools .btn {
  color: #9E9E9E;
}

.box-header > .box-tools .btn:hover {
  color: #D4A843;
}

/* ============================================
   SCROLLBAR
   ============================================ */
::-webkit-scrollbar {
  width: 6px;
  height: 6px;
}

::-webkit-scrollbar-track {
  background: #f1f1f1;
}

::-webkit-scrollbar-thumb {
  background: #c1c1c1;
  border-radius: 3px;
}

::-webkit-scrollbar-thumb:hover {
  background: #9E9E9E;
}

/* ============================================
   HIGHLIGHTED ROWS (main brand in tables)
   ============================================ */
.main-brand-row {
  background-color: rgba(212, 168, 67, 0.08) !important;
  font-weight: 600;
}

/* Disabled button and input styling */
.btn-primary[disabled],
.btn-primary.disabled {
  background: #cbd5e0 !important;
  cursor: not-allowed;
  opacity: 0.7;
  box-shadow: none !important;
  transform: none !important;
}

.form-control[disabled] {
  background-color: #f7fafc !important;
  cursor: not-allowed;
  opacity: 0.6;
}

/* ============================================
   ACCOUNT PAGE CARDS
   ============================================ */
.account-card {
  border-radius: 16px;
  padding: 25px;
  height: 100%;
  min-height: 190px;
  display: flex;
  flex-direction: column;
  justify-content: center;
  box-shadow: 0 2px 12px rgba(0,0,0,0.06);
  transition: transform 0.2s ease, box-shadow 0.2s ease;
}

.account-card:hover {
  transform: translateY(-2px);
  box-shadow: 0 6px 20px rgba(0,0,0,0.1);
}

.account-card-profile {
  background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
  color: white;
  text-align: center;
}

.account-card-gauge {
  background: white;
  text-align: center;
}

.account-card-upgrade {
  background: linear-gradient(135deg, #27AE60 0%, #2ECC71 100%);
  color: white;
  text-align: center;
}

.gauge-ring {
  position: relative;
  width: 100px;
  height: 100px;
  margin: 0 auto 10px;
}

.gauge-ring svg {
  transform: rotate(-90deg);
}

.gauge-ring .gauge-text {
  position: absolute;
  top: 50%;
  left: 50%;
  transform: translate(-50%, -50%);
  font-size: 22px;
  font-weight: 700;
}

.gauge-label {
  font-size: 11px;
  font-weight: 600;
  text-transform: uppercase;
  letter-spacing: 0.5px;
  color: #718096;
  margin-top: 4px;
}

.account-list-item {
  display: flex;
  align-items: center;
  padding: 12px 0;
  border-bottom: 1px solid #f0f0f0;
  transition: background 0.15s ease;
}

.account-list-item:hover {
  background: rgba(102, 126, 234, 0.03);
}

.account-list-item:last-child {
  border-bottom: none;
}

.account-list-icon {
  flex: 0 0 36px;
  width: 36px;
  height: 36px;
  border-radius: 8px;
  display: flex;
  align-items: center;
  justify-content: center;
  font-size: 14px;
}

.account-list-icon.brand {
  background: rgba(52, 152, 219, 0.1);
  color: #3498DB;
}

.account-list-icon.brand.pending {
  background: rgba(149, 165, 166, 0.1);
  color: #95a5a6;
}

.account-list-icon.query {
  background: rgba(102, 126, 234, 0.1);
  color: #667eea;
}

.account-list-content {
  flex: 1;
  padding: 0 12px;
  min-width: 0;
}

.account-list-content .name {
  font-weight: 500;
  font-size: 14px;
  color: #2d3748;
  white-space: nowrap;
  overflow: hidden;
  text-overflow: ellipsis;
}

.account-list-content .meta {
  font-size: 11px;
  color: #a0aec0;
  margin-top: 2px;
}

.account-list-actions {
  flex: 0 0 36px;
}

.btn-remove {
  width: 32px;
  height: 32px;
  border-radius: 8px;
  border: none;
  background: #f0f0f0;
  color: #a0aec0;
  display: flex;
  align-items: center;
  justify-content: center;
  cursor: pointer;
  transition: all 0.15s ease;
  padding: 0;
  font-size: 12px;
}

.btn-remove:hover {
  background: #fed7d7;
  color: #E74C3C;
}

.slot-badge {
  font-size: 11px;
  font-weight: 600;
  padding: 4px 10px;
  border-radius: 20px;
  letter-spacing: 0.3px;
}

.slot-badge.available {
  background: rgba(39, 174, 96, 0.1);
  color: #27AE60;
}

.slot-badge.full {
  background: rgba(231, 76, 60, 0.1);
  color: #E74C3C;
}



")
