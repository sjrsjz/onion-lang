mod protocol;
mod server;
mod capabilities;
mod document;
mod diagnostics;
mod handlers;
mod semantic;

use std::net::{TcpListener, TcpStream};
use std::io::{BufReader, BufWriter};
use std::sync::{Arc, Mutex};
use log::{info, error};


/// 启动LSP服务器
pub fn start_lsp_server(port: u16) -> Result<(), String> {
    // 初始化日志系统
    env_logger::Builder::new()
        .filter_level(log::LevelFilter::Debug)
        .init();
    
    info!("Starting Onion LSP server on port {port}...");
    
    // 创建TCP监听器
    let listener = match TcpListener::bind(format!("127.0.0.1:{port}")) {
        Ok(listener) => listener,
        Err(e) => return Err(format!("Failed to bind TCP socket on port {port}: {e}")),
    };
    
    // 等待客户端连接
    info!("LSP server started and listening on port {port}");
    println!("Onion LSP server running on port {port}");
    
    for stream in listener.incoming() {
        match stream {
            Ok(stream) => {
                info!("Client connected from {}", stream.peer_addr().unwrap_or_else(|_| "unknown".parse().unwrap()));
                
                // 启动LSP会话
                if let Err(e) = handle_client(stream) {
                    error!("Error handling client: {e}");
                }
            }
            Err(e) => {
                error!("Connection failed: {e}");
            }
        }
    }
    
    Ok(())
}

/// 处理单个LSP客户端连接
fn handle_client(stream: TcpStream) -> Result<(), String> {
    // 设置可选的超时
    if let Err(e) = stream.set_read_timeout(Some(std::time::Duration::from_secs(300))) {
        return Err(format!("Failed to set read timeout: {e}"));
    }
    
    if let Err(e) = stream.set_write_timeout(Some(std::time::Duration::from_secs(10))) {
        return Err(format!("Failed to set write timeout: {e}"));
    }
    
    info!("Creating new LSP session");
    
    // 创建服务器实例
    let server = Arc::new(Mutex::new(server::LspServer::new()));
    
    // 复制流以便读写
    match stream.try_clone() {
        Ok(writer_stream) => {
            // 使用BufReader和BufWriter增强性能
            let reader = BufReader::new(stream);
            let writer = BufWriter::new(writer_stream);
            
            // 启动LSP会话
            server::run_lsp_server(server, reader, writer)
                .map_err(|e| format!("LSP server error: {e}"))
        },
        Err(e) => Err(format!("Failed to clone TCP stream: {e}"))
    }
}